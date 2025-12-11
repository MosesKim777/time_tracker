CREATE TABLE IF NOT EXISTS cards (
    card_uid VARCHAR(255) UNIQUE NOT NULL,
    user_id INTEGER NOT NULL
);

CREATE TABLE IF NOT EXISTS work_schedules (
    user_id INTEGER NOT NULL,
    start_time TIME NOT NULL,
    end_time TIME NOT NULL,
    days INTEGER[] NOT NULL
);

CREATE TABLE IF NOT EXISTS schedule_exclusions (
    user_id INTEGER NOT NULL,
    type_exclusion VARCHAR(50) NOT NULL CHECK (type_exclusion IN ('come_later', 'leave_earlier', 'full_day')),
    start_datetime TIMESTAMP NOT NULL,
    end_datetime TIMESTAMP NOT NULL
);

CREATE TABLE IF NOT EXISTS work_history (
    user_id INTEGER NOT NULL,
    touch_time TIMESTAMP NOT NULL
);
