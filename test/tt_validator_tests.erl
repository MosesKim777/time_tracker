-module(tt_validator_tests).

-include_lib("eunit/include/eunit.hrl").
-include("errors.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================

validate_card_touch_test() ->
  ValidData = #{<<"method">> => <<"/card/touch">>, <<"card_uid">> => <<"ABC123">>},
  try
    Result = tt_validator:validate(<<"/card/touch">>, ValidData),
    ?assertEqual(<<"ABC123">>, maps:get(<<"card_uid">>, Result))
  catch
    throw:{?INVALID_REQUEST_TYPE, _} -> ?assert(false)
  end.

validate_card_touch_missing_field_test() ->
  InvalidData = #{<<"method">> => <<"/card/touch">>},
  try
    tt_validator:validate(<<"/card/touch">>, InvalidData),
    ?assert(false)
  catch
    throw:{?INVALID_REQUEST_TYPE, _} -> ?assert(true)
  end.

validate_card_assign_test() ->
  ValidData = #{
    <<"method">> => <<"/card/assign">>,
    <<"user_id">> => 1,
    <<"card_uid">> => <<"ABC123">>
  },
  try
    Result = tt_validator:validate(<<"/card/assign">>, ValidData),
    ?assertEqual(1, maps:get(<<"user_id">>, Result)),
    ?assertEqual(<<"ABC123">>, maps:get(<<"card_uid">>, Result))
  catch
    throw:{?INVALID_REQUEST_TYPE, _} -> ?assert(false)
  end.

validate_work_time_set_test() ->
  ValidData = #{
    <<"method">> => <<"/work_time/set">>,
    <<"user_id">> => 1,
    <<"start_time">> => <<"8:30:00">>,
    <<"end_time">> => <<"17:30:00">>,
    <<"days">> => [1, 2, 3, 4, 5]
  },
  try
    Result = tt_validator:validate(<<"/work_time/set">>, ValidData),
    ?assertEqual(1, maps:get(<<"user_id">>, Result)),
    ?assertEqual(<<"8:30:00">>, maps:get(<<"start_time">>, Result)),
    ?assertEqual(<<"17:30:00">>, maps:get(<<"end_time">>, Result)),
    ?assertEqual([1, 2, 3, 4, 5], maps:get(<<"days">>, Result))
  catch
    throw:{?INVALID_REQUEST_TYPE, _} -> ?assert(false)
  end.

validate_work_time_set_invalid_days_test() ->
  InvalidData = #{
    <<"method">> => <<"/work_time/set">>,
    <<"user_id">> => 1,
    <<"start_time">> => <<"8:30:00">>,
    <<"end_time">> => <<"17:30:00">>,
    <<"days">> => [1, 2, 8]
  },
  try
    tt_validator:validate(<<"/work_time/set">>, InvalidData),
    ?assert(false)
  catch
    throw:{?INVALID_REQUEST_TYPE, _} -> ?assert(true)
  end.

validate_add_exclusion_test() ->
  ValidData = #{
    <<"method">> => <<"/work_time/add_exclusion">>,
    <<"user_id">> => 1,
    <<"type_exclusion">> => <<"come_later">>,
    <<"start_datetime">> => <<"2024-01-15 08:00:00">>,
    <<"end_datetime">> => <<"2024-01-15 10:00:00">>
  },
  try
    Result = tt_validator:validate(<<"/work_time/add_exclusion">>, ValidData),
    ?assertEqual(1, maps:get(<<"user_id">>, Result)),
    ?assertEqual(<<"come_later">>, maps:get(<<"type_exclusion">>, Result))
  catch
    throw:{?INVALID_REQUEST_TYPE, _} -> ?assert(false)
  end.

validate_add_exclusion_invalid_type_test() ->
  InvalidData = #{
    <<"method">> => <<"/work_time/add_exclusion">>,
    <<"user_id">> => 1,
    <<"type_exclusion">> => <<"invalid_type">>,
    <<"start_datetime">> => <<"2024-01-15 08:00:00">>,
    <<"end_datetime">> => <<"2024-01-15 10:00:00">>
  },
  try
    tt_validator:validate(<<"/work_time/add_exclusion">>, InvalidData),
    ?assert(false)
  catch
    throw:{?INVALID_REQUEST_TYPE, _} -> ?assert(true)
  end.

validate_statistics_by_user_test() ->
  ValidData = #{
    <<"method">> => <<"/work_time/statistics_by_user">>,
    <<"user_id">> => 1,
    <<"period">> => <<"week">>
  },
  try
    Result = tt_validator:validate(<<"/work_time/statistics_by_user">>, ValidData),
    ?assertEqual(1, maps:get(<<"user_id">>, Result)),
    ?assertEqual(<<"week">>, maps:get(<<"period">>, Result))
  catch
    throw:{?INVALID_REQUEST_TYPE, _} -> ?assert(false)
  end.

validate_statistics_by_user_default_period_test() ->
  ValidData = #{
    <<"method">> => <<"/work_time/statistics_by_user">>,
    <<"user_id">> => 1
  },
  try
    Result = tt_validator:validate(<<"/work_time/statistics_by_user">>, ValidData),
    ?assertEqual(1, maps:get(<<"user_id">>, Result)),
    ?assertEqual(<<"month">>, maps:get(<<"period">>, Result))
  catch
    throw:{?INVALID_REQUEST_TYPE, _} -> ?assert(false)
  end.

validate_undefined_method_test() ->
  try
    tt_validator:validate(<<"/undefined/method">>, #{}),
    ?assert(false)
  catch
    throw:{?INVALID_REQUEST_TYPE, _} -> ?assert(true)
  end.

