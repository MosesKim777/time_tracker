-module(tt_validator).

-export([validate/2]).

-define(SCHEMA_CARD_UID, #{
  <<"card_uid">> => [required, string]
  }).

-define(SCHEMA_USER_ID, #{
  <<"user_id">> => [required, integer]
  }).

validate(Method, Data) ->
  Schema = get_schema(Method),
  case liver:validate(Schema, Data) of
    {ok, ValidData} ->
      ValidData;
    {error, Reason} ->
      throw({<<"invalid_request">>, Reason})
  end.

get_schema(<<"/card/touch">>) ->
  ?SCHEMA_CARD_UID;
get_schema(<<"/card/assign">>) ->
  #{
    <<"user_id">> => [required, integer],
    <<"card_uid">> => [required, string]
  };
get_schema(<<"/card/delete">>) ->
  ?SCHEMA_CARD_UID;
get_schema(<<"/card/list_by_user">>) ->
  ?SCHEMA_USER_ID;
get_schema(<<"/card/delete_all_by_user">>) ->
  ?SCHEMA_USER_ID;
get_schema(<<"/work_time/set">>) ->
  #{
    <<"user_id">> => [required, integer],
    <<"start_time">> => [required, string],
    <<"end_time">> => [required, string],
    <<"days">> => [required, {list_of, integer}]
  };
get_schema(<<"/work_time/get">>) ->
  ?SCHEMA_USER_ID;
get_schema(<<"/work_time/add_exclusion">>) ->
  #{
    <<"user_id">> => [required, integer],
    <<"type_exclusion">> => [required, {one_of, [[<<"come_later">>, <<"leave_earlier">>, <<"full_day">>]]}],
    <<"start_datetime">> => [required, string],
    <<"end_datetime">> => [required, string]
  };
get_schema(<<"/work_time/get_exclusion">>) ->
  ?SCHEMA_USER_ID;
get_schema(<<"/work_time/history_by_user">>) ->
  ?SCHEMA_USER_ID;
get_schema(<<"/work_time/statistics_by_user">>) ->
  #{
    <<"user_id">> => [required, integer],
    <<"period">> => [{one_of, [[<<"week">>, <<"month">>, <<"year">>, <<"all">>]]}, default, <<"month">>]
  };
get_schema(Method) ->
  throw({<<"invalid_request">>, <<"Method '", (Method)/binary, "' is undefined">>}).