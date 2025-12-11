-module(tt_db).

%% API
-export([
  get_user_id_by_card/1,
  set_touch/2,
  assign/2,
  delete/1,
  list_card_by_user/1,
  delete_all_by_user/1
]).

get_user_id_by_card(CardUid) ->
  Sql = "SELECT user_id FROM cards WHERE card_uid = $1",
  case make_request(Sql, [CardUid]) of
    {ok, _, [{UserId}]} ->
      {ok, UserId};
    {ok, _, []} ->
      throw({<<"invalid_request">>, <<"No users by 'card_uid' = ", (CardUid)/binary>>});
    {error, Reason} ->
      throw({<<"db_error">>, tt_utils:val_to_binary(Reason)})
  end.

set_touch(UserId, TouchTime) ->
  Sql = "INSERT INTO work_history (user_id, touch_time) VALUES ($1, $2)",
  case make_request(Sql, [UserId, TouchTime]) of
    {ok, _} ->
      ok;
    {error, Reason} ->
      throw({<<"db_error">>, tt_utils:val_to_binary(Reason)})
  end.

assign(CardUid, UserId) ->
  Sql = "INSERT INTO cards (card_uid, user_id) VALUES ($1, $2)",
  case make_request(Sql, [CardUid, UserId]) of
    {ok, _} ->
      ok;
    {error, Reason} ->
      throw({<<"db_error">>, tt_utils:val_to_binary(Reason)})
  end.

delete(CardUid) ->
  Sql = "DELETE FROM cards WHERE card_uid = $1 RETURNING user_id",
  case make_request(Sql, [CardUid]) of
    {ok, _, [{UserId}]} ->
      {ok, UserId};
    {ok, _, []} ->
      throw({<<"invalid_request">>, <<"There is no card with 'card_uid' = ", (CardUid)/binary>>});
    {error, Reason} ->
      throw({<<"db_error">>, tt_utils:val_to_binary(Reason)})
  end.

list_card_by_user(UserId) ->
  Sql = "SELECT card_uid FROM cards WHERE user_id = $1",
  case make_request(Sql, [UserId]) of
    {ok, _, []} ->
      throw({<<"invalid_request">>, <<"No card by 'user_id' = ",
        (integer_to_binary(UserId))/binary>>});
    {ok, _, Rows} ->
      CardUids = [CardUid || {CardUid} <- Rows],
      {ok, CardUids};
    {error, Reason} ->
      throw({<<"db_error">>, tt_utils:val_to_binary(Reason)})
  end.

delete_all_by_user(UserId) ->
  Sql = "DELETE FROM cards WHERE user_id = $1 RETURNING card_uid",
  case make_request(Sql, [UserId]) of
    {ok, _, _, []} ->
      throw({<<"invalid_request">>, <<"No card by 'user_id' = ",
        (integer_to_binary(UserId))/binary>>});
    {ok, _, _, Rows} ->
      CardUids = [CardUid || {CardUid} <- Rows],
      {ok, CardUids};
    {error, Reason} ->
      throw({<<"db_error">>, tt_utils:val_to_binary(Reason)})
  end.

make_request(Sql, Args) ->
  {ok, Connection} = tt_db_worker:get_connection(),
  epgsql:equery(Connection, Sql, Args).

