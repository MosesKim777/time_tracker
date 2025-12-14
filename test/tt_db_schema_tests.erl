-module(tt_db_schema_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST_DB_AVAILABLE).

schema_operations_test_() ->
  {setup,
   fun setup/0,
   fun cleanup/1,
   fun(SetupData) ->
     [
       test_create_schema(SetupData),
       test_clear_all_tables(SetupData),
       test_drop_schema(SetupData)
     ]
   end}.

setup() ->
  case application:ensure_all_started(time_tracker) of
    {ok, _} -> ok;
    {error, {already_started, _}} -> ok;
    Error -> throw({setup_error, Error})
  end,
  ok.

cleanup(_) ->
  ok.

test_create_schema(_) ->
  ?_test(begin
    Result = tt_db_schema:create_schema(),
    ?assertMatch(ok, Result)
  end).

test_clear_all_tables(_) ->
  ?_test(begin
    ok = tt_db_schema:create_schema(),
    Result = tt_db_schema:clear_all_tables(),
    ?assertMatch(ok, Result)
  end).

test_drop_schema(_) ->
  ?_test(begin
    ok = tt_db_schema:create_schema(),
    Result = tt_db_schema:drop_schema(),
    ?assertMatch(ok, Result)
  end).

-endif.
