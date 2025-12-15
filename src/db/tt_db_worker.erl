-module(tt_db_worker).

-behaviour(gen_server).

%% API
-export([
  start_link/0,
  get_connection/0
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-record(state, {connection}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_connection() ->
  gen_server:call(?MODULE, get_connection).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  case connect_to_db() of
    {ok, Connection} ->
      {ok, #state{connection = Connection}};
    {error, Reason} ->
      {stop, {failed_to_connect, Reason}}
  end.

handle_call(get_connection, _From, State) ->
  {reply, {ok, State#state.connection}, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  case State#state.connection of
    undefined -> ok;
    Connection -> epgsql:close(Connection)
  end,
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

connect_to_db() ->
  Config = application:get_env(time_tracker, database, []),
  Host = proplists:get_value(host, Config, "localhost"),
  Port = proplists:get_value(port, Config, 5432),
  Username = proplists:get_value(username, Config, "postgres"),
  Password = proplists:get_value(password, Config, "postgres"),
  Database = proplists:get_value(database, Config, "postgres"),

  epgsql:connect(Host, Username, Password, [
    {database, Database},
    {port, Port}
  ]).
