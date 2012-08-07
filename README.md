# Chatterbot

Chatterbot is a markov-chain based IRC bot implemented in Erlang. This version of Chatterbot learns from messages sent by other users in IRC, instead of from a training file. The longer she runs, the better her output will be.

Note that Chatterbot is, in effect, logging IRC conversations; please be certain that your server and channel permit logging of public conversations.

To use:
```
c(chatter).
PID = chatter:connect("irc.someserver.net").
PID ! {join, "#somechannel"}.
PID ! {quit}.
```

Chatterbot will respond when someone mentions her in a channel or when she is sent a private message.

Things you can (and should!) modify:

1.	Chatterbot's nickname, defined in a macro in chatter.erl.
2.	The respond_to_message/4 method. This actually invokes the markov chain output, but she could respond any way you like.