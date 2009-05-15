-module(ex_pp).
%-export([parse/1, parse/2]).
-compile(export_all).

-define(L, 4).

%% excavator pre-processor

parse(Filename) when is_list(Filename) -> parse(Filename, []).
parse(Filename, MainArgs) when is_list(Filename) ->
	case catch epp:parse_file(Filename,[],[]) of
		{ok, [A,B|Rest]} ->			
			case A of
        		{attribute,_,file,{_,_}} ->
        			ok;
        		Other ->
        			exit({bad_file_attribute, Other})
        	end,
        	
        	case B of
        		{function,_,main,Arity,[{clause,_,Args,[],Tokens}]} ->
                    Tokens1 = lists:reverse(build_instrs(Tokens, [])),
        			%ModName = string:join(string:tokens(filename:absname(Filename), "/"), "."),
        			Forms = [
        				{attribute,1,file,{Filename,1}},
        				{attribute,1,module,module_name()},
        				{attribute,2,compile,[export_all]},
        				{function,3,main,Arity,[{clause,3,Args,[],[to_cons(Tokens1)]}]}
        			|Rest],
        			{ok, Mod, Bins} = compile:forms(Forms),
        			code:load_binary(Mod, atom_to_list(Mod), Bins),
        			erlang:apply(Mod, main, MainArgs);
        		Other1 ->
        			exit({missing_main_function, Other1})
        	end;
		{'EXIT', Err} ->
			exit(Err);
		Other ->
			exit({parse_error, Other})
	end.
	
module_name() ->
    {A,B,C} = now(),
    random:seed(A,B,C),
    list_to_atom([random:uniform(26) + 96 || _ <- lists:seq(1,32)]).

build_instrs([], Acc) -> Acc;
build_instrs([Instr|Tail], Acc) ->
	build_instrs(Tail, [build_instr(Instr)|Acc]).

build_instr({call, _, {atom, _, each}, [Key, Source, Instrs]}) ->
	{tuple, ?L, [{atom,?L,instr}, {atom,?L,each}, to_cons([Key, Source, build_instr(Instrs)])]};

build_instr({call, _, {atom, _, condition}, [Condition, Instrs]}) ->
    {tuple, ?L, [{atom,?L,instr}, {atom,?L,condition}, to_cons([expand_condition(Condition), build_instr(Instrs)])]};

build_instr({call, _, {atom, _, condition}, [Condition, TrueInstrs, FalseInstrs]}) ->
    {tuple, ?L, [{atom,?L,instr}, {atom,?L,condition}, to_cons([expand_condition(Condition), build_instr(TrueInstrs), build_instr(FalseInstrs)])]};
    
build_instr({call, _, {atom, _, onfail}, [Error, Instrs]}) ->
	{tuple, ?L, [{atom,?L,instr}, {atom,?L,onfail}, to_cons([Error, build_instr(Instrs)])]};
	
build_instr({call, _, {atom, _, function}, [Function]}) ->
    {tuple, ?L, [{atom,?L,instr}, {atom,?L,function}, to_cons([Function])]};
	
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
	
expand_condition({op, _, Op, Left, Right}) ->
    {tuple, ?L, [{atom, ?L, op}, {atom, ?L, Op}, expand_condition(Left), expand_condition(Right)]};
    
expand_condition(Other) -> Other.

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