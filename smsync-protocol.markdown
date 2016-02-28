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
 * `InHeader`
 * `InBody`

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

|Current state|Response   |New state           |
|:------------|:----------|:-------------------|
|`PreHello`   |`Hello`    |`PreAuth`           |
|`PreAuth`    |`Bad`      |`PreAuth`           |
|`PreMode`    |`Bad`      |`PreMode`           |
|`PreMode`    |`Ok`       |Depends on the mode.|
|`Upload`     |`GoAhead`  |`InHeader`          |
|`Upload`     |`Ok`       |`PreMode`           |
|`InHeader`   |`GoAhead`  |`InBody`            |
|`InHeader`   |`NoThanks` |`InHeader`          |
|`InBody`     |`Ok`       |`Upload`            |
|Any state    |`Bye`      |`PreHello`          |

State `PreHello`
----------------

SMSP begins in this state. In this state, the server simply expects the client
to send the literal string `SMS\n` before proceeding responding with `Hello`
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
select. Currently, the only supported mode is `Upload`, in which the client can
upload SMS to the server. If the client requests the `Upload` mode, then the
server transitions to the `Upload` state; else, if the client requests an
unsupported mode, then the server responds with `Bad` and remains in the
`PreMode` state.

State `Upload`
--------------

From this state, the client can request to return to the `PreMode` state by
sending `End`, to which the server will respond `Ok`. Alternatively, by sending
`Upload`, the server will respond with `GoAhead` and tranition to the
`InHeader` state.

State `InHeader`
----------------

The server accepts a block of headers from the client in this state. Each
header consists of a single line of the form `XXX YYY...\n` where `XXX` is the
name of the header and `YYY...` are its parameters. Header upload ends when a
blank line is sent to the server.

The server determines whether it already has the message identifier by those
headers. If it already has the message, then it responds with `NoThanks` and
transitions to the `Upload` state; else, if it does not already have the
message, then it responds with `GoAhead` and transitions to the `InBody` state.

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

State `InBody`
--------------

This state is no longer a text-mode state. Instead, from the headers, the
server knows the expected length of the message body, and simply reads that
many bytes from the socket. After the message body is uploaded, the client must
also upload a newline `\n`. The server will respond `Ok` and transition to the
`Upload` state.
