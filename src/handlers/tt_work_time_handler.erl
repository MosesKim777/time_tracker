-module(tt_work_time_handler).

%% API
-export([
  set_work_time/1,
  get_work_time/1,
  add_exclusion/1,
  get_exclusion/1,
  history_by_user/1,
  statistics_by_user/1
]).

%%%===================================================================
%%% API
%%%===================================================================

set_work_time(Data) ->
  UserId = maps:get(<<"user_id">>, Data),
  StartTimeStr = maps:get(<<"start_time">>, Data),
  EndTimeStr = maps:get(<<"end_time">>, Data),
  Days = maps:get(<<"days">>, Data),

  {ok, StartTime} = tt_utils:parse_time(StartTimeStr),
  {ok, EndTime} = tt_utils:parse_time(EndTimeStr),

  ok = tt_db:set_work_schedule(UserId, StartTime, EndTime, Days),
  tt_utils:build_ok_response(<<>>).

get_work_time(Data) ->
  UserId = maps:get(<<"user_id">>, Data),
  {ok, {StartTime, EndTime, Days}} = tt_db:get_work_schedule(UserId),

  Response = #{
    <<"user_id">> => UserId,
    <<"start_time">> => tt_utils:format_time(StartTime),
    <<"end_time">> => tt_utils:format_time(EndTime),
    <<"days">> => Days
  },
  tt_utils:build_ok_response(Response).

add_exclusion(Data) ->
  UserId = maps:get(<<"user_id">>, Data),
  TypeExclusion = maps:get(<<"type_exclusion">>, Data),
  StartDateTimeStr = maps:get(<<"start_datetime">>, Data),
  EndDateTimeStr = maps:get(<<"end_datetime">>, Data),
  
  {ok, StartDateTime} = tt_utils:parse_datetime(StartDateTimeStr),
  {ok, EndDateTime} = tt_utils:parse_datetime(EndDateTimeStr),

  ok = tt_db:add_exclusion(UserId, TypeExclusion, StartDateTime, EndDateTime),
  tt_utils:build_ok_response(<<>>).

get_exclusion(Data) ->
  UserId = maps:get(<<"user_id">>, Data),
  {ok, Exclusions} = tt_db:get_exclusions(UserId),

  Formatted = [#{
    <<"type_exclusion">> => Type,
    <<"start_datetime">> => tt_utils:format_datetime(Start),
    <<"end_datetime">> => tt_utils:format_datetime(End)
  } || {Type, Start, End} <- Exclusions],
  
  Response = #{
    <<"user_id">> => UserId,
    <<"exclusions">> => Formatted
  },
  tt_utils:build_ok_response(Response).

history_by_user(Data) ->
  UserId = maps:get(<<"user_id">>, Data),
  {ok, History} = tt_db:get_history_by_user(UserId),

  Sorted = sorted_by_date(History),

  Response = #{
    <<"user_id">> => UserId,
    <<"history">> => Sorted
  },
  tt_utils:build_ok_response(Response).

statistics_by_user(Data) ->
  UserId = maps:get(<<"user_id">>, Data),
  Period = maps:get(<<"period">>, Data, <<"month">>),
  
  {StartDate, EndDate} = tt_utils:get_period_dates(Period, UserId),

  {ok, History} = tt_db:get_history_by_period(UserId, StartDate, EndDate),
  {ok, Schedule} = tt_db:get_work_schedule(UserId),
  {ok, Exclusions} = tt_db:get_exclusions_by_period(UserId, StartDate, EndDate),

  Stats = calculate_statistics(History, Schedule, Exclusions, StartDate, EndDate),
  
  Response = #{
    <<"user_id">> => UserId,
    <<"period">> => Period,
    <<"statistics">> => Stats
  },
  tt_utils:build_ok_response(Response).

%%%===================================================================
%%% Internal functions
%%%===================================================================

sorted_by_date(History) ->
  Sorted = tt_utils:group_by_date(History),
  io:format("Sorted: ~p~n", [Sorted]),

  [#{
    <<"date">> => tt_utils:format_date(Date),
    <<"toches_time">> => format_time(TochesTime)
  } || {Date, TochesTime} <- Sorted].

format_time(TochesTime) ->
  [tt_utils:format_time(Time) || {_, Time} <- TochesTime].


calculate_statistics(History, Schedule, Exclusions, StartDate, EndDate) ->
  ExpectedHours = calculate_expected_hours(Schedule, Exclusions, StartDate, EndDate),
  WorkedHours = calculate_worked_hours(History),
  UnderworkedHours = max(0, ExpectedHours - WorkedHours),
  
  {LateWithoutReason, LateWithReason, EarlyWithoutReason, EarlyWithReason} =
    analyze_arrivals_departures(History, Schedule, Exclusions),
  
  #{
    <<"expected_hours">> => tt_utils:format_time(tt_utils:seconds_to_hms(ExpectedHours)),
    <<"worked_hours">> => tt_utils:format_time(tt_utils:seconds_to_hms(WorkedHours)),
    <<"underworked_hours">> => tt_utils:format_time(tt_utils:seconds_to_hms(UnderworkedHours)),

    <<"late_without_reason_count">> => LateWithoutReason,
    <<"late_with_reason_count">> => LateWithReason,
    <<"early_without_reason_count">> => EarlyWithoutReason,
    <<"early_with_reason_count">> => EarlyWithReason
  }.

calculate_expected_hours({StartTime, EndTime, Days}, Exclusions, StartDate, EndDate) ->
  WorkDays = tt_utils:count_work_days(StartDate, EndDate, Days),
  HoursPerDay = tt_utils:time_diff_hours(StartTime, EndTime),
  TotalHours = WorkDays * HoursPerDay,

  ExclusionHours = calculate_exclusion_hours(Exclusions, HoursPerDay),
  TotalHours - ExclusionHours.

calculate_exclusion_hours(Exclusions, HoursPerDay) ->
  lists:sum([
    case Type of
      <<"full_day">> ->
        HoursPerDay;
      <<"come_later">> ->
        calculate_time(Start, End);
      <<"leave_earlier">> ->
        calculate_time(Start, End);
      _ ->
        0
    end
    || {Type, Start, End} <- Exclusions
  ]).

calculate_worked_hours(History) ->
  Grouped = tt_utils:group_by_date(History),
  lists:sum([calculate_day_hours(Touches) || {_, Touches} <- Grouped]).

calculate_day_hours([_Start]) ->
  0;
calculate_day_hours([Start, End | _]) ->
  calculate_time(Start, End).

calculate_time(Start, End) ->
  {_, StartTime} = Start,
  {_, EndTime} = End,
  tt_utils:time_diff_hours(StartTime, EndTime).

analyze_arrivals_departures(History,  {StartTime, EndTime, _}, Exclusions) ->
  {LateExl, EarlyExl} = count_exclusions_by_type(Exclusions),
  {AllLate, AllEarly}  = count_all_by_history(History, StartTime, EndTime),
  Late = max(0, AllLate - LateExl),
  Early = max(0, AllEarly - EarlyExl),
  {Late, LateExl, Early, EarlyExl}.

count_exclusions_by_type(Exclusions) ->
  lists:foldl(
    fun({Type, _, _}, {CL, LE}) ->
      case Type of
        <<"come_later">> ->
          {CL + 1, LE};
        <<"leave_earlier">> ->
          {CL, LE + 1};
        <<"full_day">> ->
          {CL, LE};
        _Other ->
          {CL, LE}
      end
    end, {0, 0}, Exclusions).

count_all_by_history(History, StartTime, EndTime) ->
  Grouped = tt_utils:group_by_date(History),
  lists:foldl(
    fun({_Date, [{_, InTime}, {_, OutTime}]},
        {LateAcc, EarlyAcc}) ->
      Late =
        case InTime > StartTime of
          true  -> 1;
          false -> 0
        end,
      Early =
        case OutTime < EndTime of
          true  -> 1;
          false -> 0
        end,
      {LateAcc + Late, EarlyAcc + Early}
    end, {0, 0}, Grouped).
