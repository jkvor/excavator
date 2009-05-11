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

build_instr({call, _, {atom, _, each}, [Key, Source, Instrs]}) ->
	{tuple, ?L, [{atom,?L,instr}, {atom,?L,each}, to_cons([Key, Source, build_instr(Instrs)])]};

build_instr({call, _, {atom, _, onfail}, [Error, Instrs]}) ->
	{tuple, ?L, [{atom,?L,instr}, {atom,?L,onfail}, to_cons([Error, build_instr(Instrs)])]};
	
build_instr({cons,_,Instr,{nil,_}}) ->
	{cons, ?L, build_instr(Instr), {nil,?L}};
	
build_instr({cons,_,Instr,Cons}) ->
	{cons, ?L, build_instr(Instr), build_instr(Cons)};
	
build_instr({call, _, {atom, _, Instr}, Args}) 
	when Instr==configure; Instr==assign; Instr==assert; 
		 Instr==fetch; Instr==print; Instr==function; 
		 Instr==onfail; Instr==commit ->
	{tuple, ?L, [{atom,?L,instr}, {atom,?L,Instr}, to_cons(Args)]};
	
build_instr(Instr) ->
	exit({unrecognized_instruction, Instr}).
	
preprocess_arg({tuple, _, [{atom,_,regexp},{atom,_,Key},{string,_,Regexp}]}) ->
	{ok, {re_pattern, A, B, Bin}} = re:compile(Regexp),
	Pattern = {tuple,?L,[
				{atom,?L,re_pattern},
	        	{integer,?L,A},
	        	{integer,?L,B},
	        	{bin,?L,
					[{bin_element,?L,{integer,?L,I},default,default} || I <- binary_to_list(Bin)]
				}
			  ]},
	{tuple, ?L, [{atom,?L,regexp},{atom,?L,Key},Pattern]};
	
preprocess_arg(Arg) -> Arg.
	
to_cons([]) -> {nil, ?L};
to_cons([Arg|Tail]) -> {cons,?L,preprocess_arg(Arg),to_cons(Tail)}.