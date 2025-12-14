-module(tt_utils_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================

val_to_binary_test() ->
  ?assertEqual(<<"1">>, tt_utils:val_to_binary(1)),
  ?assertEqual(<<"\"test\"">>, tt_utils:val_to_binary("test")),
  ?assertEqual(<<"{ok,value}">>, tt_utils:val_to_binary({ok, value})).

build_ok_response_test() ->
  Response = tt_utils:build_ok_response(#{<<"key">> => <<"value">>}),
  ?assertEqual(<<"ok">>, maps:get(<<"status">>, Response)),
  ?assertEqual(#{<<"key">> => <<"value">>}, maps:get(<<"data">>, Response)),
  
  EmptyResponse = tt_utils:build_ok_response(<<>>),
  ?assertEqual(<<"ok">>, maps:get(<<"status">>, EmptyResponse)),
  ?assertEqual(<<>>, maps:get(<<"data">>, EmptyResponse)).

parse_time_test() ->
  ?assertEqual({ok, {8, 30, 0}}, tt_utils:parse_time(<<"8:30">>)),
  ?assertEqual({ok, {8, 30, 0}}, tt_utils:parse_time("8:30")),
  ?assertEqual({ok, {8, 30, 15}}, tt_utils:parse_time(<<"8:30:15">>)),
  ?assertEqual({ok, {17, 45, 30}}, tt_utils:parse_time("17:45:30")),
  ?assertEqual({ok, {8, 30, 0}}, tt_utils:parse_time({8, 30, 0})),
  ?assertEqual({error, invalid_time_format}, tt_utils:parse_time(<<"invalid">>)),
  ?assertEqual({error, invalid_time_format}, tt_utils:parse_time(<<"8">>)).

format_time_test() ->
  ?assertEqual(<<"8:30:00">>, tt_utils:format_time({8, 30, 0})),
  ?assertEqual(<<"17:45:30">>, tt_utils:format_time({17, 45, 30})),
  ?assertEqual(<<"9:05:00">>, tt_utils:format_time({9, 5, 0})),
  ?assertEqual(<<"0:00:00">>, tt_utils:format_time({0, 0, 0})).

format_date_test() ->
  ?assertEqual(<<"2024-01-15">>, tt_utils:format_date({2024, 1, 15})),
  ?assertEqual(<<"2024-12-31">>, tt_utils:format_date({2024, 12, 31})),
  ?assertEqual(<<"2024-01-01">>, tt_utils:format_date({2024, 1, 1})).

format_datetime_test() ->
  ?assertEqual(<<"2024-01-15 08:30:00">>, 
               tt_utils:format_datetime({{2024, 1, 15}, {8, 30, 0}})),
  ?assertEqual(<<"2024-12-31 23:59:59">>, 
               tt_utils:format_datetime({{2024, 12, 31}, {23, 59, 59}})).

parse_datetime_test() ->
  ?assertEqual({ok, {{2024, 1, 15}, {8, 30, 0}}}, 
               tt_utils:parse_datetime(<<"2024-01-15 08:30:00">>)),
  ?assertEqual({ok, {{2024, 1, 15}, {8, 30, 0}}}, 
               tt_utils:parse_datetime("2024-01-15 08:30:00")),
  ?assertEqual({ok, {{2024, 1, 15}, {8, 30, 0}}}, 
               tt_utils:parse_datetime({{2024, 1, 15}, {8, 30, 0}})),
  ?assertEqual({error, invalid_datetime_format}, 
               tt_utils:parse_datetime(<<"invalid">>)).

time_diff_hours_test() ->
  ?assertEqual(32400, tt_utils:time_diff_hours({8, 30, 0}, {17, 30, 0})),
  ?assertEqual(3600, tt_utils:time_diff_hours({8, 0, 0}, {9, 0, 0})),
  ?assertEqual(0, tt_utils:time_diff_hours({8, 30, 0}, {8, 30, 0})),
  ?assertEqual(1800, tt_utils:time_diff_hours({8, 0, 0}, {8, 30, 0})).

seconds_to_hms_test() ->
  ?assertEqual({9, 0, 0}, tt_utils:seconds_to_hms(32400)),
  ?assertEqual({1, 0, 0}, tt_utils:seconds_to_hms(3600)),
  ?assertEqual({0, 30, 0}, tt_utils:seconds_to_hms(1800)),
  ?assertEqual({0, 0, 45}, tt_utils:seconds_to_hms(45)),
  ?assertEqual({1, 30, 45}, tt_utils:seconds_to_hms(5445)).

group_by_date_test() ->
  History = [
    {{2024, 1, 15}, {8, 30, 0}},
    {{2024, 1, 15}, {17, 30, 0}},
    {{2024, 1, 16}, {8, 30, 0}},
    {{2024, 1, 16}, {17, 30, 0}}
  ],
  Result = tt_utils:group_by_date(History),
  ?assertEqual(2, length(Result)),
  {Date1, Touches1} = lists:nth(1, Result),
  {Date2, Touches2} = lists:nth(2, Result),
  ?assert(lists:member({2024, 1, 15}, [Date1, Date2])),
  ?assert(lists:member({2024, 1, 16}, [Date1, Date2])),
  ?assertEqual(2, length(Touches1)),
  ?assertEqual(2, length(Touches2)).

