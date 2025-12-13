-module(tt_validator).

-include("time_tracker.hrl").


-export([validate/2]).

-define(SCHEMA_CARD_UID, #{
  <<"card_uid">> => [required, is_bstring]
  }).

-define(SCHEMA_USER_ID, #{
  <<"user_id">> => [required, is_integer]
  }).

%%%===================================================================
%%% API
%%%===================================================================

validate(Method, Data) ->
  Schema = get_schema(Method),
  case liver:validate(Schema, Data) of
    {ok, ValidData} ->
      ValidData;
    {error, Reason} ->
      throw({<<"invalid_request">>, Reason})
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_schema(<<"/card/touch">>) ->
  ?SCHEMA_CARD_UID;
get_schema(<<"/card/assign">>) ->
  #{
    <<"user_id">> => [required, is_integer],
    <<"card_uid">> => [required, is_bstring]
  };
get_schema(<<"/card/delete">>) ->
  ?SCHEMA_CARD_UID;
get_schema(<<"/card/list_by_user">>) ->
  ?SCHEMA_USER_ID;
get_schema(<<"/card/delete_all_by_user">>) ->
  ?SCHEMA_USER_ID;
get_schema(<<"/work_time/set">>) ->
  #{
    <<"user_id">> => [required, is_integer],
    <<"start_time">> => [required, is_bstring],
    <<"end_time">> => [required, is_bstring],
    <<"days">> =>  [required,
      {list_of, [
        {one_of, ?WEEK_DAYS}
      ]}
    ]
  };
get_schema(<<"/work_time/get">>) ->
  ?SCHEMA_USER_ID;
get_schema(<<"/work_time/add_exclusion">>) ->
  #{
    <<"user_id">> => [required, is_integer],
    <<"type_exclusion">> => [required, {one_of, [[<<"come_later">>, <<"leave_earlier">>, <<"full_day">>]]}],
    <<"start_datetime">> => [required, is_bstring],
    <<"end_datetime">> => [required, is_bstring]
  };
get_schema(<<"/work_time/get_exclusion">>) ->
  ?SCHEMA_USER_ID;
get_schema(<<"/work_time/history_by_user">>) ->
  ?SCHEMA_USER_ID;
get_schema(<<"/work_time/statistics_by_user">>) ->
  #{
    <<"user_id">> => [required, is_integer],
    <<"period">> => [
      {default, <<"month">>},
      {one_of, [<<"week">>, <<"month">>, <<"year">>, <<"all">>]}
    ]
  };
get_schema(Method) ->
  throw({<<"invalid_request">>, <<"Method '", (Method)/binary, "' is undefined">>}).