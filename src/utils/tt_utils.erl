-module(tt_utils).

-export([
  val_to_binary/1,
  build_ok_response/1,
  parse_time/1,
  parse_datetime/1,
  format_time/1,
  format_date/1,
  format_datetime/1,
  time_diff_hours/2,
  seconds_to_hms/1,
  get_period_dates/2,
  count_work_days/3,
  group_by_date/1
]).

val_to_binary(Value) ->
  list_to_binary(io_lib:format("~p", [Value])).

build_ok_response(Response) ->
  #{
    <<"status">> => <<"ok">>,
    <<"data">> => Response
  }.

parse_time(TimeStr) when is_binary(TimeStr) ->
  parse_time(binary_to_list(TimeStr));
parse_time(TimeStr) when is_list(TimeStr) ->
  case string:split(TimeStr, ":", all) of
    [H, M] ->
      {ok, {list_to_integer(H), list_to_integer(M), 0}};
    [H, M, S] ->
      S1 = string:trim(S),
      {ok, {list_to_integer(H), list_to_integer(M), list_to_integer(S1)}};
    _ ->
      {error, invalid_time_format}
  end;
parse_time({H, M, S}) ->
  {ok, {H, M, S}}.

parse_datetime(DateTimeStr) when is_binary(DateTimeStr) ->
  parse_datetime(binary_to_list(DateTimeStr));
parse_datetime(DateTimeStr) when is_list(DateTimeStr) ->
  case string:split(DateTimeStr, "T", all) of
    [DateStr, TimeStrWithZ] ->
      TimeStr = string:trim(TimeStrWithZ, trailing, "Z"),
      parse_date_time(DateStr, TimeStr);
    _ ->
      case string:split(DateTimeStr, " ", leading) of
        [DateStr, TimeStr] ->
          parse_date_time(DateStr, TimeStr);
        _ ->
          {error, invalid_datetime_format}
      end
  end;
parse_datetime({{Y, M, D}, {H, Min, S}}) ->
  {ok, {{Y, M, D}, {H, Min, S}}}.

parse_date_time(DateStr, TimeStr) ->
  try
    [Y, M, D] = [list_to_integer(X) || X <- string:split(DateStr, "-", all)],
    case string:split(TimeStr, ":", all) of
      [H, Min, S] ->
        S1 = string:trim(S),
        {ok, {{Y, M, D}, {list_to_integer(H), list_to_integer(Min), list_to_integer(S1)}}};
      [H, Min] ->
        {ok, {{Y, M, D}, {list_to_integer(H), list_to_integer(Min), 0}}};
      _ ->
        {error, invalid_time_format}
    end
  catch
    _:_ ->
      {error, invalid_datetime_format}
  end.

format_time({H, M, S}) ->
  list_to_binary(io_lib:format("~B:~2..0B:~2..0B", [H, M, trunc(S)])).

format_date({Y, M, D}) ->
  list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B", [Y, M, D])).

format_datetime({{Y, M, D}, {H, Min, S}}) ->
  list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
    [Y, M, D, H, Min, trunc(S)])).

time_diff_hours({H1, M1, S1}, {H2, M2, S2}) ->
  Seconds1 = H1 * 3600 + M1 * 60 + trunc(S1),
  Seconds2 = H2 * 3600 + M2 * 60 + trunc(S2),
  Seconds2 - Seconds1.

seconds_to_hms(TotalSeconds) ->
  Hours = TotalSeconds div 3600,
  Rem1  = TotalSeconds rem 3600,
  Minutes = Rem1 div 60,
  Seconds = Rem1 rem 60,
  {Hours, Minutes, Seconds}.

get_period_dates(<<"week">>, _) ->
  {{Y, M, D} = Date, _} = calendar:local_time(),
  WeekDay = calendar:day_of_the_week(Date),
  Start = {{Y, M, D - WeekDay + 1}, {0, 0, 0}},
  End = {{Y, M, D + 7 - WeekDay}, {23, 59, 59}},
  {Start, End};

get_period_dates(<<"month">>, _) ->
  Now = calendar:local_time(),
  {{Y, M, _}, _} = Now,
  Start = {{Y, M, 1}, {0, 0, 0}},
  LastDay = calendar:last_day_of_the_month(Y, M),
  End = {{Y, M, LastDay}, {23, 59, 59}},
  {Start, End};

get_period_dates(<<"year">>, _) ->
  Now = calendar:local_time(),
  {{Y, _, _}, _} = Now,
  Start = {{Y, 1, 1}, {0, 0, 0}},
  End = {{Y, 12, 31}, {23, 59, 59}},
  {Start, End};

get_period_dates(<<"all">>, UserId) ->
  {ok, Start} = tt_db:get_first_touch_date(UserId),
  {FirstDay, {H, M, S}} = Start,
  End = calendar:local_time(),
  {{FirstDay, {H, M, trunc(S)}}, End}.

count_work_days({{StartY, StartM, StartD}, _}, {{EndY, EndM, EndD}, _}, WorkDays) ->
  StartDays = calendar:date_to_gregorian_days(StartY, StartM, StartD),
  EndDays = calendar:date_to_gregorian_days(EndY, EndM, EndD),
  lists:sum([
    case lists:member(calendar:day_of_the_week(Y, M, D), WorkDays) of
      true -> 1;
      false -> 0
    end
    || Days <- lists:seq(StartDays, EndDays),
       {Y, M, D} <- [calendar:gregorian_days_to_date(Days)]
  ]).

group_by_date(History) ->
  Grouped = lists:foldl(fun(TouchTime, Acc) ->
    {{Y, M, D}, _} = TouchTime,
    Date = {Y, M, D},
    case maps:get(Date, Acc, undefined) of
      undefined ->
        maps:put(Date, [TouchTime], Acc);
      Existing ->
        maps:put(Date, [TouchTime | Existing], Acc)
    end
  end, #{}, History),
  [{Date, lists:sort(Touches)} || {Date, Touches} <- maps:to_list(Grouped)].
