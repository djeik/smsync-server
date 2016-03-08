The SMSync Protocol
===================

SMSync uses a simple stateful lightweight line-oriented protocol to replicate
messages across devices. The SMSync Protocol (SMSP) is designed to be used over
TCP and is designed to leave connections open for as long as possible, in order
to minimize the traffic required to establish and shutdown connections and to
allow devices to replicate messages in real-time.

SMSP uses the following states on the server side.

 * `PreHello`
 * `PreAuth`
 * `PreMode`
 * `Upload`
 * `UploadHeader`
 * `UploadBody`
 * `Confirm`
 * `ConfirmHeader`

The following responses can be issued by the server.

 * `Hello`
 * `Bye`
 * `GoAhead`
 * `NoThanks`
 * `Ok`
 * `Bad`

State breakdown
===============

This section describes each state of the protocol, the responses that the
server issues, and the state transitions that consequently occur in the server.

Here is the full transition table, followed by a breakdown of how the client
should behave in each state.

|Current state  |Response   |New state           |
|:--------------|:----------|:-------------------|
|`PreHello`     |`Hello`    |`PreAuth`           |
|`PreAuth`      |`Bad`      |`PreAuth`           |
|`PreMode`      |`Bad`      |`PreMode`           |
|`PreMode`      |`Ok`       |Depends on the mode.|
|`Upload`       |`GoAhead`  |`UploadHeader`      |
|`Upload`       |`Ok`       |`PreMode`           |
|`UploadHeader` |`GoAhead`  |`UploadBody`        |
|`UploadHeader` |`NoThanks` |`Upload`            |
|`UploadBody`   |`Ok`       |`Upload`            |
|`Confirm`      |`GoAhead`  |`ConfirmHeader`     |
|`Confirm`      |`Ok`       |`PreMode`           |
|`ConfirmHeader`|`Ok`       |`Confirm`           |
|`ConfirmHeader`|`Bad`      |`Confirm`           |
|Any state      |`Bye`      |`PreHello`          |

State `PreHello`
----------------

SMSP begins in this state. In this state, the server simply expects the client
to send the literal string `SMS\n` before proceeding to respond with `Hello`
and transitioning to the `PreAuth` state.

State `PreAuth`
---------------

The server is waiting for a password from the client. The passwords are in fact
unique tokens that associate phones with server-side accounts. Consequently, by
using the same token across multiple phones, messages can be synchronized.

The server expects a message of the form `Key XXX\n` where `XXX` is in fact the
token. If the token is associated with an account, then the server responses
with `Hello` again and transitions to the `PreMode` state; else, the server
responds with `Bad` and remains in the `PreAuth` state.

State `PreMode`
---------------

The client and server can now negociate what to do. The client selects a mode
with a message of the form `Mode XXX` where `XXX` is in fact the mode to
select. The following modes are supported.

* `Upload`: the server transitions to the `Upload` state, for receiving SMS
  from the phone.
* `Confirm`: the server transitions to `Confirm` state, in which a phone can
  send confirmations to the server that synthetic messages were dispatched.
* `Download`: the server transitions to `Download` state, in which the server
  can use the connection to submit messages to a device.

State `Upload`
--------------

From this state, the client can request to return to the `PreMode` state by
sending `End`, to which the server will respond `Ok`. Alternatively, by sending
`Upload`, the server will respond with `GoAhead` and tranition to the
`UploadHeader` state.

State `UploadHeader`
----------------

The server accepts a block of headers from the client in this state. Each
header consists of a single line of the form `XXX YYY...\n` where `XXX` is the
name of the header and `YYY...` are its parameters. Header upload ends when a
blank line is sent to the server.

The server determines whether it already has the message identifier by those
headers. If it already has the message, then it responds with `NoThanks` and
transitions to the `Upload` state; else, if it does not already have the
message, then it responds with `GoAhead` and transitions to the `UploadBody`
state.

### Required headers

The following headers are required for a message upload.

 * `Id`: a device-specific integer that acts as a unique identifier for the
   message.
 * `Time`: the time that the message was received by the phone, encoded
   according to RFC 8601, i.e. `YYYY-mm-DDTHH:MM:SS.QZ`.
 * `From`: the phone number of the sender, optionally followed by a space and
   their name.
 * `To`: the phone number of the recipient, optionally followed by a space and
   their name.
 * `Length`: the message body length

The `From` and `To` headers may, instead of a phone number, be supplied with
the special string `me`.

State `UploadBody`
--------------

This state is no longer a text-mode state. Instead, from the headers, the
server knows the expected length of the message body, and simply reads that
many bytes from the socket. After the message body is uploaded, the client must
also upload a newline `\n`. The server will respond `Ok\n` and transition to
the `Upload` state.

State `Confirm`
---------------

When the server is in this state, the client may send dispatch confirmations to
the server. Successful dispatch confirmations mark a pending message as having
been dispatched to the SMS network in the database.

The client can initiate a confirmation upload by sending `Confirm\n`, in which
case the server transitions to the `ConfirmHeader` state. The client may also
cause the server to return to the `PreMode` state by sending `End\n`.

State `ConfirmHeader`
---------------------

The server accepts a block of headers from the client in this state. Each
header consists of a single line of the form `XXX YYY...\n` where `XXX` is the
name of the header and `YYY...` are its parameters. Header upload ends when a
blank line is sent to the server.

If the confirmation makes sense, then the server responds with `Ok\n` and
transitions to the the `Confirm` state; else, if there was a problem in the
confirmation, the server responds `Bad\n` and transitions to the `Confirm`
state.

Message identifiers
===================

Message identifiers are a tricky business because there are two two different
kinds, and keeping them consistent isn't easy. Any message sent or received by
Android is given a unique identifier that is shipped to the SMSync server under
the `Id` header. The SMSync server is capable of replicating messages for
multiple devices, so these identifiers are not unique. Hence, for database
storage, the SMSync server also needs its own unique identifiers for messages.
Let's establish some terminology.

 * *ServerMID* is the unique identifier given to an uploaded or synthetic
   message by the SMSync server.
 * *ClientMID* is the unique identifier given to a message by the Android
   system.

When a message is uploaded to the SMSync server, the server records it along
with its ClientMID, associating it with a ServerMID. Notice that the pair
(ClientMID, Key) is unique. This part is relatively straightforward.

When a message is generated by a non-SMS-enabled device and is to be
dispatched, we don't know what the ClientMID is yet, so we store it to the
database with the ClientMID set to NULL and we add it to a table that acts as a
queue for message dispatching.

When a default dispatch gateway for an account connects to the SMSync server,
any synthetic messages in the dispatch queue that belong to that account are
sent to the phone and their corresponding records in the dispatch queue are
deleted.

The dispatch gateway records these to its internal database and sends them when
SMS service is available. It marks them as sent. At a later time, it sends a
confirmation message to the SMSync server (using the `Confirm` mode) containing
the generated ClientMID as well as the. The server records the ClientMID
