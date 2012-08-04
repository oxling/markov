-module(chatter).
-compile(export_all).
-import(markov).
-define(nick, "hs_lunchbot").

send(Socket, Format, Args) ->
	String = io_lib:format(Format, Args),
	io:format(String, []),
	gen_tcp:send(Socket, String).
send(Socket, String) ->
	send(Socket, String, []).

connect(Server) when is_list(Server) ->
	{ok, Socket} = gen_tcp:connect(Server, 6667, []),
	send(Socket, "NICK ~s\n", [?nick]),
	send(Socket, "USER ~s localhost ~s :~s\n", [?nick, Server, ?nick]),

	PID = spawn(?MODULE, listen, [Socket, ""]),
	link(PID),

	ok = gen_tcp:controlling_process(Socket, PID),
	PID.


listen(Socket, Remainder) ->
	receive
		{tcp, Socket, Data} ->
			{Array, Leftover} = split_str(Remainder ++ Data, []),
			lists:foreach(fun(S) -> handle(Socket, parse(S)) end, Array),
			listen(Socket, Leftover);
		{quit} ->
			gen_tcp:close(Socket);
		{join, Channel} ->
			send(Socket, "JOIN ~s\r\n", [Channel]),
			listen(Socket, Remainder)
	end.


% Returns {Results, Remainder}
% where results = array of parsed strings, 
% Remainder is any orphaned data from the next line.
split_str([], Results) ->
	{Results, ""};
split_str(String, Results) ->
	Indx = string:str(String, "\r\n"),
	case Indx of
		0 -> {Results, String};
		_ ->
			Left = string:substr(String, 1, Indx-1),
			Right = string:substr(String, Indx+2),
			split_str(Right, [Left | Results])
	end.

handle(Socket, {Source, Command, Args, Last_Arg}) ->
	io:format("~s: ~p ~p ~p\n", [Source, Command, Args, Last_Arg]),
	case {Source, Command, Args, Last_Arg} of 
		{_, "PING", _, _} -> 
			send(Socket, "PONG\r\n", []);

		% This is a general message in a channel.
		{ _, "PRIVMSG", ["#" ++ Channel | _], Msg } ->
			case directed_at_me(Msg) of
				false -> ok;
				true -> respond_to_message(Socket, "#" ++ Channel, Msg)
			end;

		% This is a private message.
		{ User, "PRIVMSG", [Channel | _], Msg } ->
			case Channel of
				?nick -> respond_to_message(Socket, short_user_name(User), Msg)
			end;

		_ -> ok
	end.

respond_to_message(Socket, Target, Message) ->
	Output = markov:write_line(markov:parse_file("training_data.txt"), Message),
	send(Socket, "PRIVMSG ~s :Lunch will be: ~s\r\n", [Target, Output]).

directed_at_me(Message) ->
	case string:str(Message, ?nick) of
		0 -> false;
		_ -> true
	end.

short_user_name(Name) ->
	string:sub_word(Name, 1, $!).

parse(":" ++ Line) ->
	%:source command args :last_arg
	Source = string:sub_word(Line, 1),
	Indx = string:chr(Line, $\s),
	case Indx of
		0 -> {"", "", "", ""}; %incomplete line, TODO
		_ -> parse(Source, string:substr(Line, Indx))
	end;
parse(Line) ->
	parse("", Line).

parse(Source, Line) ->
	%command args :last_arg
	Indx = string:chr(Line, $:),
	case Indx of
		0 -> Left = Line, Right = "";
		_ ->
			Left = string:substr(Line, 1, Indx-1),
			Right = string:substr(Line, Indx+1)
	end,

	WC = string:words(Left),
	Command = string:sub_word(Left, 1),

	case WC of
		1 -> Args = [];
		_ -> Args = lists:foldl(fun(S, Acc) -> [string:sub_word(Left, S) | Acc] end, [], lists:seq(2, WC))
	end,

	{Source, Command, Args, Right}.