-module(chatter).
-compile(export_all).
-import(markov).


% Using the chatterbot:
% PID = chatter:connect("irc.someserver.net").
% PID ! {join, "#channel"}.
% PID ! {quit}.


% The chatbot nickname that will appear in IRC.
-define(nick, "hs_lunchbot").

% Send a formatted message to the provided socket
send(Socket, Format, Args) ->
	String = io_lib:format(Format, Args),
	io:format(String, []),
	gen_tcp:send(Socket, String).
send(Socket, String) ->
	send(Socket, String, []).


% Connect to the specified server. Uses the nick defined above.
connect(Server) when is_list(Server) ->
	{ok, Socket} = gen_tcp:connect(Server, 6667, []),
	send(Socket, "NICK ~s\n", [?nick]),
	send(Socket, "USER ~s localhost ~s :~s\n", [?nick, Server, ?nick]),

	PID = spawn(?MODULE, listen, [Socket]),
	link(PID),

	ok = gen_tcp:controlling_process(Socket, PID),
	PID.

listen(Socket) ->
	listen(Socket, [], "").

% Listens and responds to input from the IRC server.
% Socket: Socket connected to IRC server
% State: Any persistent data necessary to respond to messages.
% Remainder: Text left-over from parsing. Will be pre-pended to the next packet.
listen(Socket, State, Remainder) ->
	receive
		{tcp, Socket, Data} ->
			{Array, Leftover} = split_str(Remainder ++ Data, []),
			lists:foreach(fun(S) -> handle(Socket, parse(S), State) end, Array),
			listen(Socket, State, Leftover);
		{load, File} when is_list(File) ->
			listen(Socket, markov:parse_file(File), Remainder);
		{quit} ->
			gen_tcp:close(Socket);
		{join, Channel} ->
			send(Socket, "JOIN ~s\r\n", [Channel]),
			listen(Socket, State, Remainder)
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

% Responds to IRC messages.
handle(Socket, {Source, Command, Args, Last_Arg}, State) ->
	io:format("~s: ~p ~p ~p\n", [Source, Command, Args, Last_Arg]),
	case {Source, Command, Args, Last_Arg} of 
		{_, "PING", _, _} -> 
			send(Socket, "PONG\r\n", []);

		% This is a general message in a channel.
		{ _, "PRIVMSG", ["#" ++ Channel | _], Msg } ->
			case directed_at_me(Msg) of
				false -> ok;
				true -> respond_to_message(Socket, "#" ++ Channel, Msg, State)
			end;

		% This is a private message.
		{ User, "PRIVMSG", [?nick | _], Msg } ->
			respond_to_message(Socket, short_user_name(User), Msg, State);

		% Ignore everything else.
		_ -> ok
	end.

% Respond to an IRC message directed at the chatbot.
% Target: The target channel or user
% Message: What the user said
% State: Current chatbot state
respond_to_message(Socket, Target, Message, State) ->
	Output = markov:write_line(State, Message),
	send(Socket, "PRIVMSG ~s :Today we'll be eating: ~s\r\n", [Target, Output]).

% Checks whether nor not a message references the chatbot
directed_at_me(Message) ->
	case string:str(Message, ?nick) of
		0 -> false;
		_ -> true
	end.

% Trims a long-form user name to just the relevant (visible) portion
short_user_name(Name) ->
	string:sub_word(Name, 1, $!).

% Parse IRC messages.
% Returns {Source, Command, Args, Last_Arg}
% Example:
%
% :User COMMAND arg1 arg2 arg3 :extended information
% returns {"User", "COMMAND", ["arg1", "arg2", "arg3"], "extended information"}

parse(":" ++ Line) ->
	% This is a message with a source
	% :source command args :last_arg
	Source = string:sub_word(Line, 1),
	Indx = string:chr(Line, $\s),
	case Indx of
		0 -> {"", "", "", ""}; %incomplete line, TODO
		_ -> parse(Source, string:substr(Line, Indx))
	end;
parse(Line) ->
	parse("", Line).

parse(Source, Line) ->
	% This is a message without a source (or the source has been parsed)
	% command args :last_arg
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