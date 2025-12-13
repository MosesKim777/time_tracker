-module(tt_db).

-include("errors.hrl").

%% API
-export([
  get_user_id_by_card/1,
  set_touch/2,
  assign/2,
  delete/1,
  list_card_by_user/1,
  delete_all_by_user/1,
  set_work_schedule/4,
  get_work_schedule/1,
  add_exclusion/4,
  get_exclusions/1,
  get_exclusions_by_period/3,
  get_history_by_user/1,
  get_history_by_period/3,
  get_first_touch_date/1
]).

%%%===================================================================
%%% API
%%%===================================================================

get_user_id_by_card(CardUid) ->
  Sql = "SELECT user_id FROM cards WHERE card_uid = $1",
  case make_request(Sql, [CardUid]) of
    {ok, _, [{UserId}]} ->
      {ok, UserId};
    {ok, _, []} ->
      throw({?INVALID_REQUEST_TYPE, ?ERROR_NO_USERS_BY_CARD_UID(CardUid)});
    {error, Reason} ->
      throw({?DB_ERROR_TYPE, tt_utils:val_to_binary(Reason)})
  end.

set_touch(UserId, TouchTime) ->
  Sql = "INSERT INTO work_history (user_id, touch_time) VALUES ($1, $2)",
  case make_request(Sql, [UserId, TouchTime]) of
    {ok, _} ->
      ok;
    {error, Reason} ->
      throw({?DB_ERROR_TYPE, tt_utils:val_to_binary(Reason)})
  end.

assign(CardUid, UserId) ->
  Sql = "INSERT INTO cards (card_uid, user_id) VALUES ($1, $2)",
  case make_request(Sql, [CardUid, UserId]) of
    {ok, _} ->
      ok;
    {error, Reason} ->
      throw({?DB_ERROR_TYPE, tt_utils:val_to_binary(Reason)})
  end.

delete(CardUid) ->
  Sql = "DELETE FROM cards WHERE card_uid = $1 RETURNING user_id",
  case make_request(Sql, [CardUid]) of
    {ok, _, _, [{UserId}]} ->
      {ok, UserId};
    {ok, _, _, []} ->
      throw({?INVALID_REQUEST_TYPE, ?ERROR_NO_CARDS_BY_CARD_UID(CardUid)});
    {error, Reason} ->
      throw({?DB_ERROR_TYPE, tt_utils:val_to_binary(Reason)})
  end.

list_card_by_user(UserId) ->
  Sql = "SELECT card_uid FROM cards WHERE user_id = $1",
  case make_request(Sql, [UserId]) of
    {ok, _, []} ->
      throw({?INVALID_REQUEST_TYPE, ?ERROR_NO_CARDS_BY_USER_ID(UserId)});
    {ok, _, Rows} ->
      CardUids = [CardUid || {CardUid} <- Rows],
      {ok, CardUids};
    {error, Reason} ->
      throw({?DB_ERROR_TYPE, tt_utils:val_to_binary(Reason)})
  end.

delete_all_by_user(UserId) ->
  Sql = "DELETE FROM cards WHERE user_id = $1 RETURNING card_uid",
  case make_request(Sql, [UserId]) of
    {ok, _, _, []} ->
      throw({?INVALID_REQUEST_TYPE, ?ERROR_NO_CARDS_BY_USER_ID(UserId)});
    {ok, _, _, Rows} ->
      CardUids = [CardUid || {CardUid} <- Rows],
      {ok, CardUids};
    {error, Reason} ->
      throw({?DB_ERROR_TYPE, tt_utils:val_to_binary(Reason)})
  end.

set_work_schedule(UserId, StartTime, EndTime, Days) ->
  DeleteSql = "DELETE FROM work_schedules WHERE user_id = $1",
  InsertSql = "INSERT INTO work_schedules (user_id, start_time, end_time, days) VALUES ($1, $2::time, $3::time, $4)",
  case make_request(DeleteSql, [UserId]) of
    {ok, _} ->
      case make_request(InsertSql, [UserId, StartTime, EndTime, Days]) of
        {ok, _} -> ok;
        {error, Reason} -> throw({?DB_ERROR_TYPE, tt_utils:val_to_binary(Reason)})
      end;
    {error, Reason} ->
      throw({?DB_ERROR_TYPE, tt_utils:val_to_binary(Reason)})
  end.


get_work_schedule(UserId) ->
  Sql = "SELECT start_time, end_time, days FROM work_schedules WHERE user_id = $1",
  case make_request(Sql, [UserId]) of
    {ok, _, []} ->
      throw({?INVALID_REQUEST_TYPE, ?ERROR_NO_SCHEDULE_BY_USER_ID(UserId)});
    {ok, _, [{StartTime, EndTime, Days}]} ->
      {ok,  {StartTime, EndTime, Days}};
    {error, Reason} ->
      throw({?DB_ERROR_TYPE, tt_utils:val_to_binary(Reason)})
  end.

add_exclusion(UserId, TypeExclusion, StartDateTime, EndDateTime) ->
  Sql = "INSERT INTO schedule_exclusions (user_id, type_exclusion, start_datetime, end_datetime) VALUES ($1, $2, $3::timestamp, $4::timestamp)",
  case make_request(Sql, [UserId, TypeExclusion, StartDateTime, EndDateTime]) of
    {ok, _} -> ok;
    {error, Reason} ->
      throw({?DB_ERROR_TYPE, tt_utils:val_to_binary(Reason)})
  end.

get_exclusions(UserId) ->
  Sql = "SELECT type_exclusion, start_datetime, end_datetime FROM schedule_exclusions WHERE user_id = $1 ORDER BY start_datetime DESC",
  case make_request(Sql, [UserId]) of
    {ok, _, Exclusions} ->
      {ok, Exclusions};
    {error, Reason} ->
      throw({?DB_ERROR_TYPE, tt_utils:val_to_binary(Reason)})
  end.

get_exclusions_by_period(UserId, StartDate, EndDate) ->
  Sql = "SELECT type_exclusion, start_datetime, end_datetime FROM schedule_exclusions
   WHERE user_id = $1 AND start_datetime <= $3::timestamp AND end_datetime >= $2::timestamp",
  case make_request(Sql, [UserId, StartDate, EndDate]) of
    {ok, _, Exclusions} ->
      {ok, Exclusions};
    {error, Reason} ->
      throw({?DB_ERROR_TYPE, tt_utils:val_to_binary(Reason)})
  end.

get_history_by_user(UserId) ->
  Sql = "SELECT touch_time FROM work_history WHERE user_id = $1 ORDER BY touch_time DESC",
  case make_request(Sql, [UserId]) of
    {ok, _, Rows} ->
      History = [TouchTime || {TouchTime} <- Rows],
      {ok, History};
    {error, Reason} ->
      throw({?DB_ERROR_TYPE, tt_utils:val_to_binary(Reason)})
  end.

get_history_by_period(UserId, StartDate, EndDate) ->
  Sql = "SELECT touch_time FROM work_history WHERE user_id = $1 AND touch_time >= $2::timestamp AND touch_time <= $3::timestamp ORDER BY touch_time ASC",
  case make_request(Sql, [UserId, StartDate, EndDate]) of
    {ok, _, Rows} ->
      History = [TouchTime || {TouchTime} <- Rows],
      {ok, History};
    {error, Reason} ->
      throw({?DB_ERROR_TYPE, tt_utils:val_to_binary(Reason)})
  end.

get_first_touch_date(UserId) ->
  Sql = "SELECT MIN(touch_time) FROM work_history WHERE user_id = $1",
  case make_request(Sql, [UserId]) of
    {ok, _, [{FirstTouchDate}]} ->
      {ok, FirstTouchDate};
    {ok, _, []} ->
      throw({?INVALID_REQUEST_TYPE, ?ERROR_NO_HISTORY_BY_USER_ID(UserId)});
    {error, Reason} ->
      throw({?DB_ERROR_TYPE, tt_utils:val_to_binary(Reason)})
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

make_request(Sql, Args) ->
  {ok, Connection} = tt_db_worker:get_connection(),
  epgsql:equery(Connection, Sql, Args).