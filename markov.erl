-module(markov).
-compile(export_all).

-record(chain, {words, next=[]}).

% Generate forward and backward chains from a corpus of training data
parse_file(File_Name) when is_list(File_Name)->
	{ok, File} = file:open(File_Name, [read]),
	Lines = append_line(File, []),
	Forward_Data = train(Lines, [], forward),
	Backward_Data = train(Lines, [], backward),
	{Forward_Data, Backward_Data}.

% Output a line
write_line({[], []}) ->
	print_output([]);
write_line({Forward_Data, Backward_Data}) ->
	Forward = emit(forward, Forward_Data, []),
	Output = emit(backward, Backward_Data, lists:reverse(Forward)),
	print_output(Output).

% Output a line based on some input data.
write_line({Forward_Data, Backward_Data}, Input) ->
	% Find all the interesting keywords in the input
	Keys = lists:filter(fun(S) -> (is_word(S)) andalso (is_interesting(S)) end, symbols(Input, [])),

	% Find all grams that have their middle word matching a key
	Grams = [Gram || {chain, Gram, _} <- Forward_Data],
	Key_Grams = [X || X <- lists:map(fun(X) -> lists:keyfind(X, 2, Grams) end, Keys),
												 X /= false],
	
	case Key_Grams of
		[] -> write_line({Forward_Data, Backward_Data}); % No matches, random output
		_ -> 
			% Generate output based on a random matching gram
			{W1, W2, W3} = lists:nth(random:uniform(length(Key_Grams)), Key_Grams),
			Forward = emit(forward, Forward_Data, [W1, W2, W3]),
			Output = emit(backward, Backward_Data, lists:reverse(Forward)),
			print_output(Output)
	end.

print_output(Output) ->
	[string:join(lists:map(fun(X) -> atom_to_list(X) end, Output), "")].

is_alphanumeric(Char) ->
	case string:chr("ABCDEFGHIJKLMNOPQRSTUVWXYZ" ++ 
									"abcdefghijklmnopqrstuvwxyz" ++
									"1234567890'", Char) of
		0 -> false;
		_ -> true
	end.

is_word(Sym) when is_atom(Sym) ->
	is_word(atom_to_list(Sym));
is_word(Sym) when is_list(Sym) ->
	lists:foldl(fun(C, true) -> is_alphanumeric(C); (_C, false) -> false end, true, Sym).

is_interesting(Sym) when is_list(Sym) ->
	is_interesting(list_to_atom(Sym));
is_interesting(Sym) when is_atom(Sym) ->
	case Sym of
		'IN' -> false;
		'ON' -> false;
		'OF' -> false;
		'THE' -> false;
		'IS' -> false;
		'ARE' -> false;
		'AND' -> false;
		'TO' -> false;
		'LIKE' -> false;
		_ -> true
	end.


atomize(String) ->
	list_to_atom(string:to_upper(String)).

% Turns a line of text into atomized symbols
symbols(Line, Symbols)  when length(Line) > 0 ->
	Found = lists:takewhile(fun(C) -> markov:is_alphanumeric(C) end, Line),
	case Found of 
		"" ->
			Word = lists:takewhile(fun(C) -> not(markov:is_alphanumeric(C)) end, Line);
		_  ->
			Word = Found
	end,
	Len = length(Word),
	symbols(string:substr(Line, Len+1, length(Line)-Len), [atomize(Word) | Symbols]);

symbols(_Line, Symbols) ->
	lists:reverse(Symbols).


append_line(File, Array) ->
	case io:get_line(File, "") of
		eof -> 
			ok = file:close(File),
			Array;
		Line when is_list(Line) ->
			Clean_Line = string:strip(Line, right, $\n),
			Symbols = symbols(Clean_Line, []),
			append_line(File, [Symbols | Array])
	end.

emit(backward, Data, Output) ->
	lists:reverse(emit(forward, Data, Output));

emit(forward, Data, []) ->
	%find a random first word
	Idx = random:uniform(length(Data)),
	Chain = lists:nth(Idx, Data),
	{W1, W2, W3} = Chain#chain.words,
	case W3 of
		eof -> [W1, W2];
		_ -> emit(forward, Data, [W1, W2, W3])
	end;

emit(forward, Data, Output) when length(Output) > 2 ->
	[W1, W2, W3] = lists:sublist(Output, length(Output)-2, 3),
	Chain = lists:keyfind({W1, W2, W3}, #chain.words, Data),
	Symbol = lists:nth(random:uniform(length(Chain#chain.next)), Chain#chain.next),
	case Symbol of
		eof -> Output;
		_ -> emit(forward, Data, Output ++ [Symbol])
	end;

emit(forward, _Data, Output) ->
	Output.

% Update the markov chain, using an array of lines
train([Line | Rest], Data, forward) ->
	train(Rest, train_line(Line, Data), forward);
train(Lines, Data, backward) ->
	train(lists:map(fun(L) -> lists:reverse(L) end, Lines), Data, forward);
train([], Data, _direction) ->
	Data.

% Update the markov chain with a line of text.
train_line(Line, Data) when length(Line) > 3 ->
	[W1, W2, W3, W4 | Rest] = Line,
	Chain = lists:keyfind({W1, W2, W3}, #chain.words, Data),
	case is_record(Chain, chain) of
		false ->
			% This is a new triple.
			Next_Word = W4,
			New_Chain = #chain{words={W1, W2, W3}, next=[Next_Word]};
		true ->
			% This triple already exists
			New_Chain = #chain{words={W1, W2, W3}, next=[W4 | Chain#chain.next]}
	end,
	New_Data = lists:keystore({W1, W2, W3}, #chain.words, Data, New_Chain),

	case W4 of
		eof -> New_Data;
		_ -> train_line([W2, W3, W4 | Rest], New_Data)
	end;

train_line(Line, Data) when length(Line) > 2 ->
	train_line(Line ++ [eof], Data);
train_line(_, Data) ->
	Data.
