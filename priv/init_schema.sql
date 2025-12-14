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
    type_exclusion VARCHAR(50) NOT NULL
        CHECK (type_exclusion IN ('come_later', 'leave_earlier', 'full_day')),
    start_datetime TIMESTAMP NOT NULL,
    end_datetime TIMESTAMP NOT NULL
);

CREATE TABLE IF NOT EXISTS work_history (
    user_id INTEGER NOT NULL,
    touch_time TIMESTAMP NOT NULL
);


CREATE OR REPLACE FUNCTION check_work_history_limit()
RETURNS TRIGGER AS $$
BEGIN
    IF (
        SELECT COUNT(*)
        FROM work_history
        WHERE user_id = NEW.user_id
          AND DATE(touch_time) = DATE(NEW.touch_time)
    ) >= 2 THEN
        RAISE EXCEPTION
            'Only two work_history records allowed per user per day (user_id=% date=%)',
            NEW.user_id,
            DATE(NEW.touch_time);
    END IF;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;


DO $$
BEGIN
    IF NOT EXISTS (
        SELECT 1
        FROM pg_trigger
        WHERE tgname = 'work_history_limit_trigger'
    ) THEN
        CREATE TRIGGER work_history_limit_trigger
        BEFORE INSERT ON work_history
        FOR EACH ROW
        EXECUTE FUNCTION check_work_history_limit();
    END IF;
END;
$$;
