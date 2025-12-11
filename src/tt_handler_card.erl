-module(tt_handler_card).

%% API
-export([
  touch/1,
  assign/1,
  delete/1,
  list_by_user/1,
  delete_all_by_user/1
]).

touch(Data) ->
  CardUid = maps:get(<<"card_uid">>, Data),
  {ok, UserId} = tt_db:get_user_id_by_card(CardUid),
  TouchTime = calendar:local_time(),
  ok = tt_db:set_touch(UserId, TouchTime),
  Data = #{
    <<"card_id">> => CardUid,
    <<"user_id">> => UserId
  },
  build_ok_response(Data).

assign(Data) ->
  CardUid = maps:get(<<"card_uid">>, Data),
  UserId = maps:get(<<"user_id">>, Data),
  ok = tt_db:assign(CardUid, UserId),
  build_ok_response(<<>>).

delete(Data) ->
  CardUid = maps:get(<<"card_uid">>, Data),
  {ok, UserId} = tt_db:delete(CardUid),
  Data = #{
    <<"card_id">> => CardUid,
    <<"user_id">> => UserId
  },
  build_ok_response(Data).

list_by_user(Data) ->
  UserId = maps:get(<<"user_id">>, Data),
  {ok, CardUids} = tt_db:list_card_by_user(UserId),
  Data = #{
    <<"user_id">> => UserId,
    <<"cards">> => CardUids
  },
  build_ok_response(Data).

delete_all_by_user(Data) ->
  UserId = maps:get(<<"user_id">>, Data),
  {ok, CardUids} = tt_db:delete_all_by_user(Data),
  Data = #{
    <<"user_id">> => UserId,
    <<"cards">> => CardUids
  },
  build_ok_response(Data).

build_ok_response(Data) ->
  #{
    <<"status">> => <<"ok">>,
    <<"data">> => Data
  }.
