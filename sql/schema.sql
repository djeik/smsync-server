BEGIN;

DROP VIEW IF EXISTS confirmed_message;
DROP VIEW IF EXISTS dispatched_message;
DROP VIEW IF EXISTS undispatched_message;
DROP VIEW IF EXISTS convenient_message;
DROP VIEW IF EXISTS convenient_device;
DROP VIEW IF EXISTS convenient_account;
DROP TABLE IF EXISTS dispatch_queue;
DROP TABLE IF EXISTS message;
ALTER TABLE IF EXISTS account DROP CONSTRAINT fkey_account_dispatch_gateway_id;
DROP TABLE IF EXISTS device;
DROP TABLE IF EXISTS account;

CREATE TABLE account (
    id SERIAL PRIMARY KEY,
    name TEXT NOT NULL,
    email TEXT NOT NULL,
    key TEXT NOT NULL, -- account password
    dispatch_gateway_id INTEGER
);

CREATE TABLE device (
    id SERIAL PRIMARY KEY,
    key TEXT NOT NULL, -- device password
    is_sms_enabled BOOLEAN NOT NULL,
    account_id INTEGER REFERENCES account ( id ) ON DELETE SET NULL,
    phone_number TEXT -- null for non-SMS-enabled devices
);

ALTER TABLE account
    ADD CONSTRAINT fkey_account_dispatch_gateway_id
    FOREIGN KEY ( dispatch_gateway_id )
    REFERENCES device ( id );

CREATE TABLE message (
    id SERIAL PRIMARY KEY, -- this is the ServerMID
    dispatch_id INTEGER, -- this is the ClientMID
    device_id INTEGER NOT NULL REFERENCES device ( id ) ON DELETE CASCADE,
    sender TEXT NOT NULL,
    recipient TEXT NOT NULL,
    body TEXT NOT NULL,
    is_synthetic BOOLEAN NOT NULL
);

CREATE TABLE dispatch_queue (
    message_id INTEGER NOT NULL REFERENCES message ( id )
);

-- A view for an account along with its dispatch gateway, if any.
CREATE VIEW convenient_account (
    id,
    name,
    email,
    account_key,
    device_id,
    device_key,
    is_sms_enabled,
    phone_number
) AS
SELECT
    a.id,
    a.name,
    a.email,
    a.key,
    d.id,
    d.key,
    d.is_sms_enabled,
    d.phone_number
FROM account a LEFT OUTER JOIN device d ON a.dispatch_gateway_id = d.id;

CREATE VIEW convenient_device (
    id,
    key,
    is_sms_enabled,
    phone_number,
    account_id,
    account_name,
    account_email,
    account_key,
    gateway_id,
    gateway_key,
    gateway_is_sms_enabled,
    gateway_phone_number
) AS
SELECT
    d.id,
    d.key,
    d.is_sms_enabled,
    d.phone_number,
    a.id,
    a.name,
    a.email,
    a.key,
    g.id,
    g.key,
    g.is_sms_enabled,
    g.phone_number
FROM device d
    LEFT OUTER JOIN account a ON a.id = d.account_id
    LEFT OUTER JOIN device g ON g.id = a.dispatch_gateway_id;

CREATE VIEW convenient_message (
    id,
    dispatch_id,
    sender,
    recipient,
    is_synthetic,
    device_id,
    device_key,
    device_is_sms_enabled,
    device_phone_number,
    account_id,
    account_name,
    account_email,
    account_key,
    gateway_id,
    gateway_key,
    gateway_is_sms_enabled,
    gateway_phone_number,
    body
) AS
SELECT
    m.id,
    m.dispatch_id,
    m.sender,
    m.recipient,
    m.is_synthetic,
    d.id,
    d.key,
    d.is_sms_enabled,
    d.phone_number,
    d.account_id,
    d.account_name,
    d.account_email,
    d.account_key,
    d.gateway_id,
    d.gateway_key,
    d.gateway_is_sms_enabled,
    d.gateway_phone_number,
    m.body
FROM message m
    LEFT OUTER JOIN convenient_device d ON m.device_id = d.id;

CREATE VIEW dispatched_message (
    id,
    dispatch_id,
    sender,
    recipient,
    is_synthetic,
    device_id,
    device_key,
    device_is_sms_enabled,
    device_phone_number,
    account_id,
    account_name,
    account_email,
    account_key,
    gateway_id,
    gateway_key,
    gateway_is_sms_enabled,
    gateway_phone_number,
    body
) AS
SELECT
    m.id,
    m.dispatch_id,
    m.sender,
    m.recipient,
    m.is_synthetic,
    m.device_id,
    m.device_key,
    m.device_is_sms_enabled,
    m.device_phone_number,
    m.account_id,
    m.account_name,
    m.account_email,
    m.account_key,
    m.gateway_id,
    m.gateway_key,
    m.gateway_is_sms_enabled,
    m.gateway_phone_number,
    m.body
FROM convenient_message m
    WHERE NOT EXISTS (
        SELECT 1 FROM dispatch_queue q WHERE q.message_id = m.id
    );

CREATE VIEW undispatched_message (
    id,
    dispatch_id,
    sender,
    recipient,
    is_synthetic,
    device_id,
    device_key,
    device_is_sms_enabled,
    device_phone_number,
    account_id,
    account_name,
    account_email,
    account_key,
    gateway_id,
    gateway_key,
    gateway_is_sms_enabled,
    gateway_phone_number,
    body
) AS
SELECT
    m.id,
    m.dispatch_id,
    m.sender,
    m.recipient,
    m.is_synthetic,
    m.device_id,
    m.device_key,
    m.device_is_sms_enabled,
    m.device_phone_number,
    m.account_id,
    m.account_name,
    m.account_email,
    m.account_key,
    m.gateway_id,
    m.gateway_key,
    m.gateway_is_sms_enabled,
    m.gateway_phone_number,
    m.body
FROM dispatch_queue q
    INNER JOIN convenient_message m ON m.id = q.message_id;

CREATE VIEW confirmed_message (
    id,
    dispatch_id,
    sender,
    recipient,
    is_synthetic,
    device_id,
    device_key,
    device_is_sms_enabled,
    device_phone_number,
    account_id,
    account_name,
    account_email,
    account_key,
    gateway_id,
    gateway_key,
    gateway_is_sms_enabled,
    gateway_phone_number,
    body
) AS
SELECT
    m.id,
    m.dispatch_id,
    m.sender,
    m.recipient,
    m.is_synthetic,
    m.device_id,
    m.device_key,
    m.device_is_sms_enabled,
    m.device_phone_number,
    m.account_id,
    m.account_name,
    m.account_email,
    m.account_key,
    m.gateway_id,
    m.gateway_key,
    m.gateway_is_sms_enabled,
    m.gateway_phone_number,
    m.body
FROM convenient_message m
    WHERE m.dispatch_id IS NOT NULL;

COMMIT;
