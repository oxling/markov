# Chatterbot
Chatterbot is a markov-chain based IRC bot implemented in Erlang.

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
3.	Her corpus of training data. This is currently set to training_data.txt.