-record(state, {
    instructions=[], %% [instr()]
    dictionary=dict:new(), %% term store
    parent, %% state()
	fail, %% {Error::tuple(), FailInstructions::list()}
	request_times = [], %% [seconds()]
    configuration=dict:new()
}).

-define(DBG_REPORT(Args), ex_logger:debug_report(Args)).
-define(DBG_MSG(Format, Args), ex_logger:debug_msg(Format, Args)).

-define(INFO_REPORT(Args), ex_logger:info_report(Args)).
-define(INFO_MSG(Format, Args), ex_logger:info_msg(Format, Args)).

-define(WARNING_REPORT(Args), ex_logger:warning_report(Args)).
-define(WARNING_MSG(Format, Args), ex_logger:warning_msg(Format, Args)).

-define(ERR_REPORT(Args), ex_logger:error_report(Args)).
-define(ERR_MSG(Format, Args), ex_logger:error_msg(Format, Args)).

-define(CRITICAL_REPORT(Args), ex_logger:critical_report(Args)).
-define(CRITICAL_MSG(Format, Args), ex_logger:critical_msg(Format, Args)).

-define(ADD, fun ex_util:add/3).
-define(STORE, fun ex_util:store/3).
-define(STORE_VALUE, fun ex_util:store_value/3).
-define(GLOBAL_STORE, fun ex_util:global_store/3).
-define(FETCH, fun ex_util:fetch/2).
-define(FETCH_VALUE, fun ex_util:fetch_value/2).
-define(CONFIGURE, fun ex_util:configure/3).
-define(FETCH_CONFIG, fun ex_util:fetch_config/2).    
-define(EVALUATE, fun ex_util:evaluate/2).