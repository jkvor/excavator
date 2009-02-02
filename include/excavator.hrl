-record(assign, {function, from, to}).
-record(assert, {name, type}).
-record(commit, {name, type}).
-record(print, {format, args}).
-record(each, {name, commands}).
-record(url, {value}).
-record(file, {name}).
-record(xpath, {value}).
-record(regexp, {value}).

-define(INFO_MSG, fun(Format0, Args0) -> io:format(Format0, Args0) end).
%-define(INFO_MSG, fun(Format, Args) -> error_logger:info_msg(Format, Args) end).
-define(ERR_MSG, fun(Format, Args) -> io:format(Format, Args) end).
%-define(ERROR_MSG, fun(Format, Args) -> error_logger:error_msg(Format, Args) end).