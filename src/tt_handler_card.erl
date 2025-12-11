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
  Response = #{
    <<"card_id">> => CardUid,
    <<"user_id">> => UserId
  },
  build_ok_response(Response).

assign(Data) ->
  CardUid = maps:get(<<"card_uid">>, Data),
  UserId = maps:get(<<"user_id">>, Data),
  ok = tt_db:assign(CardUid, UserId),
  build_ok_response(<<>>).

delete(Data) ->
  CardUid = maps:get(<<"card_uid">>, Data),
  {ok, UserId} = tt_db:delete(CardUid),
  Response = #{
    <<"card_id">> => CardUid,
    <<"user_id">> => UserId
  },
  build_ok_response(Response).

list_by_user(Data) ->
  UserId = maps:get(<<"user_id">>, Data),
  {ok, CardUids} = tt_db:list_card_by_user(UserId),
  Response = #{
    <<"user_id">> => UserId,
    <<"cards">> => CardUids
  },
  build_ok_response(Response).

delete_all_by_user(Data) ->
  UserId = maps:get(<<"user_id">>, Data),
  {ok, CardUids} = tt_db:delete_all_by_user(UserId),
  Response = #{
    <<"user_id">> => UserId,
    <<"cards">> => CardUids
  },
  build_ok_response(Response).

build_ok_response(Response) ->
  #{
    <<"status">> => <<"ok">>,
    <<"data">> => Response
  }.
