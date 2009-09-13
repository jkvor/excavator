%% Copyright (c) 2009 Jacob Vorreuter <jacob.vorreuter@gmail.com>
%% 
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%% 
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
-module(ex_pp).
-export([parse/1, parse/2]).
-export([purge/1, delete/1]).

-define(L, 4).

%% excavator pre-processor

parse(Filename) when is_list(Filename) -> parse(Filename, []).
parse(Filename, MainArgs) when is_list(Filename) ->
	ModuleName = compile(Filename),
	erlang:apply(ModuleName, main, MainArgs).
	
compile(Filename) ->
    ModuleName = module_name(Filename, application:get_env(excavator, randomize_module_names)),
    case code:is_loaded(ModuleName) of
        {file, _} -> ok;
        false ->
            case catch epp:parse_file(Filename,[],[]) of
                {ok, Forms} -> 
                    {_,Indices} = lists:foldl(
                        fun ({attribute,_,file,{_,_}}, {I, Acc}) ->
                                {I+1, [{attribute, I}|Acc]};
                            ({function,_,main,_,_}, {I, Acc}) ->
                                {I+1, [{main, [I | proplists:get_value(main, Acc, [])]}|Acc]};
                            (_, {I, Acc}) ->
                                {I+1, Acc}
                        end, {1, []}, Forms),

                    case proplists:get_value(attribute, Indices) of
                        1 -> ok;
                        _ -> exit({error, bad_file_attribute})
                    end,

                    case proplists:get_value(main, Indices) of
                        undefined -> exit({error, missing_main_function});
                        _ -> ok
                    end,

                    Forms1 = 
                        [{attribute,1,file,{Filename,1}},
                         {attribute,1,module, ModuleName},
                         {attribute,1,compile,[export_all]}] ++
                         parse_include() ++
                        [process_root_level_form(Form) || Form <- lists:nthtail(1, Forms)],

                    {ok, Mod, Bins} = compile:forms(Forms1, [verbose,report_errors]),
                    code:load_binary(Mod, atom_to_list(Mod), Bins);
                {'EXIT', Err} -> 
                    exit(Err);
                Other -> 
                    exit({parse_error, Other})
            end
    end,
	ModuleName.
    
purge(Filename) ->
    ModuleName = module_name(Filename, application:get_env(excavator, randomize_module_names)),
    code:purge(ModuleName).
    
delete(Filename) ->
    ModuleName = module_name(Filename, application:get_env(excavator, randomize_module_names)),
    code:delete(ModuleName).
    
parse_include() ->
    Filename = case code:lib_dir(excavator) of
        {error, bad_name} -> "./include/excavator.hrl";
        LibDir -> LibDir ++ "/include/excavator.hrl"
    end,
    case epp:parse_file(Filename,[],[]) of
        {ok, Forms} ->
            lists:foldl(fun
                ({attribute,_,record,_}=Record, Acc) ->
                    [Record|Acc];
                (_, Acc) ->
                    Acc
                end, [], Forms);
        _ ->
            []
    end.
    
process_root_level_form({function,L,main,Arity,Clauses}) ->
    {function,L,main,Arity,[begin
        {I1, Instrs} = lists:foldl(fun
            ({tuple,_,[{atom,_,instr}|_]}=Instr, {I, Acc}) ->
                {I+1, [assign_instr(Instr, I, L1)|Acc]};
            (Other, {I, Acc}) ->
                {I, [Other|Acc]}
            end, 
            {1, [{match,L1,{var,L1,acc_var(1)},{nil,L1}}]}, 
            build_instrs(Tokens)),
        Instrs1 = lists:reverse([{var,L1,acc_var(I1)}|Instrs]),
        {clause,L1,Args,Guards,Instrs1}
    end || {clause,L1,Args,Guards,Tokens} <- Clauses]};
process_root_level_form(Form) -> Form.

acc_var(I) ->
    list_to_atom(lists:concat(["InternalExcavatorInstructionsAcc", I])).
    
assign_instr(Instr, I, L) ->
    {match,L,{var,L,acc_var(I+1)},{call,L,{remote,L,{atom,L,lists},{atom,L,append}},[{var,L,acc_var(I)},{cons,L,Instr,{nil,L}}]}}.
	
module_name(_, {ok, true}) ->
    {A,B,C} = now(),
    random:seed(A,B,C),
    list_to_atom([random:uniform(26) + 96 || _ <- lists:seq(1,32)]);

module_name(Filename, _) ->
    list_to_atom(binary_to_list(erlang:md5(Filename))).

build_instrs(Tokens) ->
    [build_instr(Token) || Token <- Tokens].

build_cons_instrs({cons,_,Instr,{nil,_}}) ->
	{cons, ?L, build_instr(Instr), {nil,?L}};

build_cons_instrs({cons,_,Instr,Cons}) ->
	{cons, ?L, build_instr(Instr), build_cons_instrs(Cons)}.
    	
build_instr({call, _, {atom, _, call}, [Module, Function, Args]}) ->
	{tuple, ?L, [{atom,?L,instr}, {atom,?L,call}, to_cons([expand_arg(Module), expand_arg(Function), expand_arg(Args)])]};
	
build_instr({call, _, {atom, _, each}, [Key, Source, Instrs]}) ->
	{tuple, ?L, [{atom,?L,instr}, {atom,?L,each}, to_cons([Key, expand_arg(Source), build_cons_instrs(Instrs)])]};

build_instr({call, _, {atom, _, condition}, [Condition, Instrs]}) ->
    {tuple, ?L, [{atom,?L,instr}, {atom,?L,condition}, to_cons([expand_condition(Condition), build_cons_instrs(Instrs)])]};

build_instr({call, _, {atom, _, condition}, [Condition, TrueInstrs, FalseInstrs]}) ->
    {tuple, ?L, [{atom,?L,instr}, {atom,?L,condition}, to_cons([expand_condition(Condition), build_cons_instrs(TrueInstrs), build_cons_instrs(FalseInstrs)])]};
    
build_instr({call, _, {atom, _, onfail}, [Error, Instrs]}) ->
	{tuple, ?L, [{atom,?L,instr}, {atom,?L,onfail}, to_cons([Error, build_cons_instrs(Instrs)])]};

build_instr({call, _, {atom, _, onfail}, [Error, Instrs, FailInstrs]}) ->
	{tuple, ?L, [{atom,?L,instr}, {atom,?L,onfail}, to_cons([Error, build_cons_instrs(Instrs), build_cons_instrs(FailInstrs)])]};
	
build_instr({call, _, {atom, _, function}, [Function]}) ->
    {tuple, ?L, [{atom,?L,instr}, {atom,?L,function}, to_cons([Function])]};
		
build_instr({call, _, {atom, _, assert}, [Condition]}) ->
    {tuple, ?L, [{atom,?L,instr}, {atom,?L,assert}, to_cons([expand_condition(Condition)])]};

build_instr({call, _, {atom, _, configure}, [qps, Value]}) when is_float(Value) ->
	validate_qps_value(Value),
	{tuple, ?L, [{atom,?L,instr}, {atom,?L,configure}, to_cons(expand_args([qps, Value]))]};
    
build_instr({call, _, {atom, _, configure}, [qps, Value, Prefix]}) when is_float(Value) ->
	validate_qps_value(Value),
	{tuple, ?L, [{atom,?L,instr}, {atom,?L,configure}, to_cons(expand_args([qps, Value, Prefix]))]};
	
build_instr({call, _, {atom, _, Instr}, Args}) when Instr == configure; 
                                                    Instr == assign; 
                                                    Instr == assert; 
                                                    Instr == print; 
                                                    Instr == function; 
                                                    Instr == onfail; 
                                                    Instr == commit; 
                                                    Instr == add;
                                                    Instr == gassign; 
                                                    Instr == gadd ->
	{tuple, ?L, [{atom,?L,instr}, {atom,?L,Instr}, to_cons(expand_args(Args))]};

build_instr({Type, _, _} = Term) when Type == atom; 
                                      Type == tuple; 
                                      Type == integer;
                                      Type == float;
                                      Type == bin;
                                      %%Type == var;
                                      Type == string ->
    {tuple, ?L, [{atom,?L,instr}, {atom,?L,term}, to_cons([Term])]};
    
build_instr({cons, _, _, _} = Cons) ->
    {tuple, ?L, [{atom,?L,instr}, {atom,?L,term}, to_cons([Cons])]};
    
build_instr(Term) -> Term.
	
expand_condition({op, _, Op, Left, Right}) ->
    {tuple, ?L, [{atom, ?L, op}, {atom, ?L, Op}, expand_condition(Left), expand_condition(Right)]};
    
expand_condition(Other) -> expand_arg(Other).

expand_args(Args) ->
    [expand_arg(Arg) || Arg <- Args].
    
expand_arg({call, _, {atom, _, xpath}, [Key, XPath]}) ->
    {tuple, ?L, [{atom,?L,xpath}, Key, expand_arg(XPath)]};

expand_arg({call, _, {atom, _, http}, [Url]}) ->
	{tuple, ?L, [{atom,?L,http_req}, {atom,?L,get}, expand_arg(Url), {nil,?L}, {nil,?L}, {nil,?L}]};
		
expand_arg({call, _, {atom, _, http}, [Method, Url]}) ->
	{tuple, ?L, [{atom,?L,http_req}, expand_arg(Method), expand_arg(Url), {nil,?L}, {nil,?L}, {nil,?L}]};
		
expand_arg({call, _, {atom, _, http}, [Method, Url, Headers]}) ->
	{tuple, ?L, [{atom,?L,http_req}, expand_arg(Method), expand_arg(Url), expand_arg(Headers), {nil,?L}, {nil,?L}]};
	
expand_arg({call, _, {atom, _, http}, [Method, Url, Headers, Body]}) ->
	{tuple, ?L, [{atom,?L,http_req}, expand_arg(Method), expand_arg(Url), expand_arg(Headers), expand_arg(Body), {nil,?L}]};

expand_arg({call, _, {atom, _, http}, [Method, Url, Headers, Body, Options]}) ->
	{tuple, ?L, [{atom,?L,http_req}, expand_arg(Method), expand_arg(Url), expand_arg(Headers), expand_arg(Body), expand_arg(Options)]};
	
expand_arg({call, _, {atom, _, regexp}, [Key, Regexp]}) ->
    {tuple, ?L, [{atom,?L,regexp}, Key, expand_arg(Regexp)]};

expand_arg({call, _, {atom, _, first}, [Key]}) ->
    {tuple, ?L, [{atom,?L,first}, expand_arg(Key)]};

expand_arg({call, _, {atom, _, last}, [Key]}) ->
    {tuple, ?L, [{atom,?L,last}, expand_arg(Key)]};

expand_arg({call, _, {atom, _, concat}, [Key]}) ->
    {tuple, ?L, [{atom,?L,concat}, expand_arg(Key)]};

expand_arg({call, _, {atom, _, range}, [Start, End]}) ->
    {tuple, ?L, [{atom,?L,range}, expand_arg(Start), expand_arg(End)]};

expand_arg({call, _, {atom, _, range}, [Start, End, Fun]}) ->
    {tuple, ?L, [{atom,?L,range}, expand_arg(Start), expand_arg(End), expand_arg(Fun)]};

expand_arg({call, _, {atom, _, read_file}, [Filename]}) ->
	{tuple, ?L, [{atom,?L,file}, expand_arg(Filename)]};
		
expand_arg({call, _, {atom, _, open_file}, [Filename, Modes]}) ->
	{tuple, ?L, [{atom,?L,open}, expand_arg(Filename), expand_arg(Modes)]};
	
expand_arg({call, _, {atom, _, call}, [Module, Function, Args]}) ->
	{tuple, ?L, [{atom,?L,call}, expand_arg(Module), expand_arg(Function), expand_arg(Args)]};
	
expand_arg(Other) -> Other.

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

validate_qps_value(Value) ->
	Multiplied = Value * 10,
	case (Multiplied - trunc(Multiplied)) of
		0 ->
			ok;
		_ ->
			exit({bad_qps_value, {Value, "must be a multiple of 0.1"}})
	end.