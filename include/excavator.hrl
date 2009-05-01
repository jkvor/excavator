-record(file, {name}).
-record(range, {current, stop, incr_fun}).
-record(xpath, {value}).
-record(regexp, {value}).
-record(state, {
    instructions=[], %% [instr()]
    dictionary=dict:new(), %% term store
    stack=[], %% [state()]
    configuration=dict:new()
}).

-define(INFO_MSG, fun(Format0, Args0) -> io:format(Format0, Args0) end).
%-define(INFO_MSG, fun(Format, Args) -> error_logger:info_msg(Format, Args) end).
-define(ERR_MSG, fun(Format, Args) -> io:format(Format, Args) end).
%-define(ERROR_MSG, fun(Format, Args) -> error_logger:error_msg(Format, Args) end).

-define(STORE, 
    fun(#state{dictionary=D}=S, K, V) ->
        S#state{dictionary=dict:store(K, V, D)}
    end).
    
-define(FETCH,
    fun(#state{dictionary=D}, K) ->
        case dict:find(K, D) of
            {ok, V} -> V;
            error -> undefined
        end
    end).
    
-define(CONFIGURE,
    fun(#state{configuration=C}=S, K, V) ->
        S#state{configuration=dict:store(K, V, C)}
    end).