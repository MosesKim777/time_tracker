-module(tt_db_schema).

%% API
-export([
  create_schema/0,
  drop_schema/0,
  clear_all_tables/0
]).

-define(TABLES, [
  "cards",
  "work_schedules",
  "schedule_exclusions",
  "work_history"
]).

%%%===================================================================
%%% API
%%%===================================================================

create_schema() ->
  case read_schema_file() of
    {ok, Sql} ->
      execute_sql(Sql);
    {error, Reason} ->
      {error, Reason}
  end.

drop_schema() ->
  {ok, Connection} = tt_db_worker:get_connection(),
  drop_tables(Connection, ?TABLES).

clear_all_tables() ->
  {ok, Connection} = tt_db_worker:get_connection(),
  clear_tables(Connection, ?TABLES).

%%%===================================================================
%%% Internal functions
%%%===================================================================

read_schema_file() ->
  SchemaPath = filename:join([code:priv_dir(time_tracker), "init_schema.sql"]),
  case file:read_file(SchemaPath) of
    {ok, Binary} ->
      {ok, binary_to_list(Binary)};
    {error, Reason} ->
      {error, {file_read_error, Reason}}
  end.

execute_sql(Sql) ->
  {ok, Connection} = tt_db_worker:get_connection(),
  case epgsql:squery(Connection, Sql) of
    Results when is_list(Results) ->
      case check_results(Results) of
        ok ->
          ok;
        {error, Reason} ->
          {error, {sql_execution_error, Reason}}
      end;
    {ok, _} ->
      ok;
    {error, Reason} ->
      {error, {sql_execution_error, Reason}}
  end.

check_results([]) ->
  ok;
check_results([{ok, _, _} | Rest]) ->
  check_results(Rest);
check_results([{ok, _} | Rest]) ->
  check_results(Rest);
check_results([{error, Reason} | _]) ->
  {error, Reason};
check_results([_ | Rest]) ->
  check_results(Rest).

drop_tables(_Connection, []) ->
  ok;
drop_tables(Connection, [TableName | Rest]) ->
  Sql = "DROP TABLE IF EXISTS " ++ TableName ++ " CASCADE",
  case epgsql:squery(Connection, Sql) of
    {ok, _, _} ->
      drop_tables(Connection, Rest);
    {ok, _} ->
      drop_tables(Connection, Rest);
    {error, Reason} ->
      {error, {drop_table_error, TableName, Reason}}
  end.

clear_tables(_Connection, []) ->
  ok;
clear_tables(Connection, TableNames) ->
  TableList = string:join(TableNames, ", "),
  Sql = "TRUNCATE TABLE " ++ TableList ++ " RESTART IDENTITY CASCADE",
  case epgsql:squery(Connection, Sql) of
    {ok, _, _} ->
      ok;
    {ok, _} ->
      ok;
    {error, Reason} ->
      {error, {truncate_error, Reason}}
  end.

