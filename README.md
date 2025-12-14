# Time Tracker

Microservice for tracking employee working hours using NFC cards.

## Description

The microservice is designed to track employee working hours using NFC cards. An NFC card is not an access pass, but is used only for employee identification. An employee must register the card only twice per day: when arriving at work and when leaving work.

Administrators have the ability to:
- View employee work history
- Set work schedules
- Add exceptions to the schedule: days off, permitted late arrivals, or early departures

## Technologies

- **Erlang/OTP** - main language and platform
- **RabbitMQ** - RPC server for request processing
- **PostgreSQL** - database
- **JSON** - data exchange format (request/response)

## Requirements

- Erlang/OTP
- PostgreSQL
- RabbitMQ
- rebar3

## Installation and Running

### Building the Project

```bash
$ rebar3 compile
```

### Configuration

Configure database and RabbitMQ connections in the `config/sys.config` file:

```erlang
[
  {time_tracker, [
    {database, [
      {host, "localhost"},
      {port, 5432},
      {username, "postgres"},
      {password, "postgres"},
      {database, "time_tracker"}
    ]},
    {rabbitmq, [
      {host, "localhost"},
      {port, 5672},
      {username, "guest"},
      {password, "guest"},
      {vhost, "/"},
      {connection_timeout, 60000}
    ]}
  ]}
].
```

### Database Initialization

```erlang
% Create database schema
tt_db_schema:create_schema().

% Clear all tables
tt_db_schema:clear_all_tables().

% Drop database schema
tt_db_schema:drop_schema().
```

### Running

```bash
$ rebar3 shell
```

### RPC Client

For testing the service, an RPC client module `tt_rpc_client.erl` has been added. This module sends requests through RabbitMQ to the `time_tracker_rpc` queue and receives responses. The client is used for all API method examples in this documentation.

The client automatically handles:
- JSON encoding/decoding of requests and responses
- Connection management to RabbitMQ
- RPC call/response pattern

All API examples in this documentation use `tt_rpc_client:call/1` to interact with the service.

## API Methods

All methods use JSON format for requests and responses. Input data validation is performed automatically.

### Card Management

#### `/card/touch`

Register a card in the system (employee "tapped" the card on the reader).

**Required fields:**
- `card_uid` (string) - unique card identifier

**Example request:**
```erlang
tt_rpc_client:call(#{
  <<"method">> => <<"/card/touch">>,
  <<"card_uid">> => <<"ABC123">>
}).
```

**Example response:**
```erlang
#{
  <<"status">> => <<"ok">>,
  <<"data">> => #{
    <<"card_id">> => <<"ABC123">>,
    <<"user_id">> => 1
  }
}
```

#### `/card/assign`

Assign a card to an employee.

**Required fields:**
- `user_id` (integer) - user identifier
- `card_uid` (string) - unique card identifier

**Example request:**
```erlang
tt_rpc_client:call(#{
  <<"method">> => <<"/card/assign">>,
  <<"user_id">> => 1,
  <<"card_uid">> => <<"ABC123">>
}).
```

**Example response:**
```erlang
#{
  <<"status">> => <<"ok">>,
  <<"data">> => <<>>
}
```

#### `/card/delete`

Delete a card.

**Required fields:**
- `card_uid` (string) - unique card identifier

**Example request:**
```erlang
tt_rpc_client:call(#{
  <<"method">> => <<"/card/delete">>,
  <<"card_uid">> => <<"ABC123">>
}).
```

**Example response:**
```erlang
#{
  <<"status">> => <<"ok">>,
  <<"data">> => #{
    <<"card_id">> => <<"ABC123">>,
    <<"user_id">> => 1
  }
}
```

#### `/card/list_by_user`

Get a list of all cards assigned to an employee.

**Required fields:**
- `user_id` (integer) - user identifier

**Example request:**
```erlang
tt_rpc_client:call(#{
  <<"method">> => <<"/card/list_by_user">>,
  <<"user_id">> => 1
}).
```

**Example response:**
```erlang
#{
  <<"status">> => <<"ok">>,
  <<"data">> => #{
    <<"user_id">> => 1,
    <<"cards">> => [<<"ABC123">>, <<"DEF456">>]
  }
}
```

#### `/card/delete_all_by_user`

Delete all cards assigned to an employee.

**Required fields:**
- `user_id` (integer) - user identifier

**Example request:**
```erlang
tt_rpc_client:call(#{
  <<"method">> => <<"/card/delete_all_by_user">>,
  <<"user_id">> => 1
}).
```

**Example response:**
```erlang
#{
  <<"status">> => <<"ok">>,
  <<"data">> => #{
    <<"user_id">> => 1,
    <<"cards">> => [<<"ABC123">>, <<"DEF456">>]
  }
}
```

### Work Time

#### `/work_time/set`

Set employee working hours.

**Required fields:**
- `user_id` (integer) - user identifier
- `start_time` (string) - work start time in "HH:MM:SS" format (e.g., "8:30:00")
- `end_time` (string) - work end time in "HH:MM:SS" format (e.g., "17:30:00")
- `days` (array of integers) - week days (1-Monday, 2-Tuesday, ..., 7-Sunday)

**Example request:**
```erlang
tt_rpc_client:call(#{
  <<"method">> => <<"/work_time/set">>,
  <<"user_id">> => 1,
  <<"start_time">> => <<"8:30:00">>,
  <<"end_time">> => <<"17:30:00">>,
  <<"days">> => [1, 2, 3, 4, 5]
}).
```

**Example response:**
```erlang
#{
  <<"status">> => <<"ok">>,
  <<"data">> => <<>>
}
```

#### `/work_time/get`

Get employee working hours.

**Required fields:**
- `user_id` (integer) - user identifier

**Example request:**
```erlang
tt_rpc_client:call(#{
  <<"method">> => <<"/work_time/get">>,
  <<"user_id">> => 1
}).
```

**Example response:**
```erlang
#{
  <<"status">> => <<"ok">>,
  <<"data">> => #{
    <<"user_id">> => 1,
    <<"start_time">> => <<"8:30:00">>,
    <<"end_time">> => <<"17:30:00">>,
    <<"days">> => [1, 2, 3, 4, 5]
  }
}
```

#### `/work_time/add_exclusion`

Add an exception to the work schedule.

**Required fields:**
- `user_id` (integer) - user identifier
- `type_exclusion` (string) - exclusion type:
  - `"come_later"` - arrive later
  - `"leave_earlier"` - leave earlier
  - `"full_day"` - full working day (day off)
- `start_datetime` (string) - start date and time in "YYYY-MM-DD HH:MM:SS" format
- `end_datetime` (string) - end date and time in "YYYY-MM-DD HH:MM:SS" format

**Example request:**
```erlang
tt_rpc_client:call(#{
  <<"method">> => <<"/work_time/add_exclusion">>,
  <<"user_id">> => 1,
  <<"type_exclusion">> => <<"come_later">>,
  <<"start_datetime">> => <<"2024-01-15 08:00:00">>,
  <<"end_datetime">> => <<"2024-01-15 10:00:00">>
}).
```

**Example response:**
```erlang
#{
  <<"status">> => <<"ok">>,
  <<"data">> => <<>>
}
```

#### `/work_time/get_exclusion`

Get all exceptions from the work schedule.

**Required fields:**
- `user_id` (integer) - user identifier

**Example request:**
```erlang
tt_rpc_client:call(#{
  <<"method">> => <<"/work_time/get_exclusion">>,
  <<"user_id">> => 1
}).
```

**Example response:**
```erlang
#{
  <<"status">> => <<"ok">>,
  <<"data">> => #{
    <<"user_id">> => 1,
    <<"exclusions">> => [
      #{
        <<"type_exclusion">> => <<"come_later">>,
        <<"start_datetime">> => <<"2024-01-15 08:00:00">>,
        <<"end_datetime">> => <<"2024-01-15 10:00:00">>
      }
    ]
  }
}
```

#### `/work_time/history_by_user`

Get employee history.

**Required fields:**
- `user_id` (integer) - user identifier

**Example request:**
```erlang
tt_rpc_client:call(#{
  <<"method">> => <<"/work_time/history_by_user">>,
  <<"user_id">> => 1
}).
```

**Example response:**
```erlang
#{
  <<"status">> => <<"ok">>,
  <<"data">> => #{
    <<"user_id">> => 1,
    <<"history">> => [
      #{
        <<"date">> => <<"2024-01-15">>,
        <<"toches_time">> => [<<"8:30:00">>, <<"17:30:00">>]
      }
    ]
  }
}
```

#### `/work_time/statistics_by_user`

Get employee statistics.

**Required fields:**
- `user_id` (integer) - user identifier

**Optional fields:**
- `period` (string) - period for data filtering:
  - `"week"` - week
  - `"month"` - month (default)
  - `"year"` - year
  - `"all"` - all period

**Example request:**
```erlang
tt_rpc_client:call(#{
  <<"method">> => <<"/work_time/statistics_by_user">>,
  <<"user_id">> => 1,
  <<"period">> => <<"week">>
}).
```

**Example response:**
```erlang
#{
  <<"status">> => <<"ok">>,
  <<"data">> => #{
    <<"user_id">> => 1,
    <<"period">> => <<"week">>,
    <<"statistics">> => #{
      <<"expected_hours">> => <<"44:45:00">>,
      <<"worked_hours">> => <<"44:30:00">>,
      <<"underworked_hours">> => <<"0:15:00">>,
      <<"late_without_reason_count">> => 1,
      <<"late_with_reason_count">> => 1,
      <<"early_without_reason_count">> => 0,
      <<"early_with_reason_count">> => 0
    }
  }
}
```

**Statistics fields:**
- `expected_hours` - expected working hours (hours:minutes:seconds)
- `worked_hours` - actual worked hours (hours:minutes:seconds)
- `underworked_hours` - underworked hours (hours:minutes:seconds)
- `late_without_reason_count` - number of late arrivals without reason
- `late_with_reason_count` - number of late arrivals with reason
- `early_without_reason_count` - number of early departures without reason
- `early_with_reason_count` - number of early departures with reason

## Error Handling

When an error occurs, the service returns a response in the following format:

```erlang
#{
  <<"status">> => <<"error">>,
  <<"error_type">> => <<"invalid_request">>,
  <<"message">> => <<"Error description">>
}
```

## Limitations

- An employee can register a card only twice per day (arrival and departure)
- Attempting to register a card more than twice per day will throw an exception

## Project Structure

```
time_tracker/
├── config/              # Configuration files
├── include/             # Header files (.hrl)
├── priv/                # Private files (SQL schemas)
├── src/
│   ├── db/              # Database modules
│   ├── handlers/        # Request handlers
│   ├── mq/              # RabbitMQ modules
│   ├── utils/           # Utilities
│   └── validation/      # Data validation
└── rebar.config         # rebar3 configuration
```

## License

Apache-2.0
