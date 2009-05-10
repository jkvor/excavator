-module(ex_pp).
%-export([parse/1]).
-compile(export_all).

-define(L, 4).

%% excavator pre-processor

parse(Filename) when is_list(Filename) ->
	case catch epp:parse_file(Filename,[],[]) of
		{ok, Tokens} ->
			Tokens1 = validate([file_attr,function_main], Tokens),
			Tokens2 = lists:reverse(build_instrs(Tokens1, [])),
			ModName = string:join(string:tokens(filename:absname(Filename), "/"), "."),
			Forms = [
				{attribute,1,file,{Filename,1}},
				{attribute,1,module,list_to_atom(ModName)},
				{attribute,2,export,[{main,0}]},
				{function,3,main,0,[{clause,3,[],[],[to_cons(Tokens2)]}]}],
			{ok, Mod, Bins} = compile:forms(Forms),
			code:load_binary(Mod, atom_to_list(Mod), Bins),
			erlang:apply(Mod, main, []);
		{'EXIT', Err} ->
			exit(Err);
		Other ->
			exit({parse_error, Other})
	end.
			
validate([file_attr|TailVals], [Token|TailTokens]) ->
	case Token of
		{attribute,_,file,{_,_}} ->
			validate(TailVals, TailTokens);
		Other ->
			exit({bad_file_attribute, Other})
	end;
	
validate([function_main|_TailVals], [Token|_TailTokens]) ->
	case Token of
		{function,_,main,0,[{clause,_,[],[],Tokens}]} ->
			Tokens;
		Other ->
			exit({missing_main_function, Other})
	end;
	
validate([Val|_], _) ->
	exit({validation_failed, Val}).

build_instrs([], Acc) -> Acc;
build_instrs([Instr|Tail], Acc) ->
	build_instrs(Tail, [build_instr(Instr)|Acc]).

build_instr({call, _, {atom, _, Instr}, Args}) ->
	{tuple, ?L, [{atom,?L,instr}, {atom,?L,Instr}, to_cons(Args)]};
	
build_instr(Instr) ->
	exit({unrecognized_instruction, Instr}).
	
to_cons([]) -> {nil, ?L};
to_cons([Arg|Tail]) -> {cons,?L,Arg,to_cons(Tail)}.