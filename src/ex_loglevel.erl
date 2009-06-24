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
-module(ex_loglevel).

-export([set/1]).

%% Error levels:
-define(LOG_LEVELS, [{no_log, 0},
                     {critical, 1},
                     {error, 2},
                     {warning, 3},
                     {info, 4},
                     {debug, 5}]).
                     
set(LogLevel) when is_atom(LogLevel) ->
    {Mod,Code} = dynamic_compile:from_string(logger_src(proplists:get_value(LogLevel, ?LOG_LEVELS, info))),
    code:load_binary(Mod, "ex_logger.erl", Code).
       
logger_src(LogLevel) when is_integer(LogLevel) ->
    L = integer_to_list(LogLevel),
    "-module(ex_logger).

    -export([debug_report/1,
             debug_msg/2,
             info_report/1,
             info_msg/2,
             warning_report/1,
             warning_msg/2,
             error_report/1,
             error_msg/2,
             critical_report/1,
             critical_msg/2]).

    debug_report(Args) when " ++ L ++ " >= 5 -> error_logger:info_report(Args);
    debug_report(_) -> ok.
    
    debug_msg(Format, Args) when " ++ L ++ " >= 5 -> error_logger:info_msg(Format, Args);
    debug_msg(_, _) -> ok.

    info_report(Args) when " ++ L ++ " >= 4 -> error_logger:info_report(Args);
    info_report(_) -> ok.
    
    info_msg(Format, Args) when " ++ L ++ " >= 4 -> error_logger:info_msg(Format, Args);
    info_msg(_, _) -> ok.
    
    warning_report(Args) when " ++ L ++ " >= 3 -> error_logger:warning_report(Args);
    warning_report(_) -> ok.
    
    warning_msg(Format, Args) when " ++ L ++ " >= 3 -> error_logger:warning_msg(Format, Args);
    warning_msg(_, _) -> ok.
    
    error_report(Args) when " ++ L ++ " >= 2 -> error_logger:error_report(Args);
    error_report(_) -> ok.
    
    error_msg(Format, Args) when " ++ L ++ " >= 2 -> error_logger:error_msg(Format, Args);
    error_msg(_, _) -> ok.

    critical_report(Args) when " ++ L ++ " >= 1 -> error_logger:error_report(Args);
    critical_report(_) -> ok.
    
    critical_msg(Format, Args) when " ++ L ++ " >= 1 -> error_logger:error_msg(Format, Args);
    critical_msg(_, _) -> ok.
    
    ".
