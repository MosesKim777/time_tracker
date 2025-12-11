-module(tt_rabbitmq_worker).

-behaviour(gen_server).

-include_lib("amqp_client/include/amqp_client.hrl").
-include("time_tracker.hrl").

%% API
-export([
  start_link/0,
  get_connection/0,
  get_channel/0
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

-record(state, {connection, channel}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_connection() ->
  gen_server:call(?MODULE, get_connection).

get_channel() ->
  gen_server:call(?MODULE, get_channel).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  case connect_to_rabbitmq() of
    {ok, Connection} ->
      case amqp_connection:open_channel(Connection) of
        {ok, Channel} ->
          {ok, #state{connection = Connection, channel = Channel}};
        {error, Reason} ->
          amqp_connection:close(Connection),
          {stop, {failed_to_open_channel, Reason}}
      end;
    {error, Reason} ->
      {stop, {failed_to_connect, Reason}}
  end.

handle_call(get_connection, _From, State) ->
  {reply, {ok, State#state.connection}, State};
handle_call(get_channel, _From, State) ->
  {reply, {ok, State#state.channel}, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  case State#state.channel of
    undefined -> ok;
    Channel ->
      cleanup_queue(Channel),
      amqp_channel:close(Channel)
  end,
  case State#state.connection of
    undefined -> ok;
    Connection -> amqp_connection:close(Connection)
  end,
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

connect_to_rabbitmq() ->
  Config = application:get_env(time_tracker, rabbitmq, []),
  Host = proplists:get_value(host, Config, "localhost"),
  Port = proplists:get_value(port, Config, 5672),
  Username = proplists:get_value(username, Config, "guest"),
  Password = proplists:get_value(password, Config, "guest"),
  VHost = proplists:get_value(vhost, Config, "/"),
  Timeout = proplists:get_value(connection_timeout, Config, 60000),

  Params = #amqp_params_network{
    host = Host,
    port = Port,
    username = list_to_binary(Username),
    password = list_to_binary(Password),
    virtual_host = list_to_binary(VHost),
    connection_timeout = Timeout
  },

  amqp_connection:start(Params).

cleanup_queue(Channel) ->
  Delete = #'queue.delete'{
    queue = ?QUEUE,
    if_unused = false,
    if_empty = false
  },
  amqp_channel:call(Channel, Delete).
