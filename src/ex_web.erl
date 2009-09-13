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
-module(ex_web).
-export([request/5]).

-include("excavator.hrl").

-define(HEADERS, [
    {"Accept","text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"},
    {"Accept-Language","en-us,en;q=0.5"},
    {"Accept-Charset","ISO-8859-1,utf-8;q=0.7,*;q=0.7"},
    {"Connection","keep-alive"},
    {"User-Agent","Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.5; en-US; rv:1.9.0.5) Gecko/2008120121 Firefox/3.0.5"}
]).

request(Method, Url, [], Body, Options) -> request(Method, Url, ?HEADERS, Body, Options);

request(Method, Url, Headers, [], Options) 
 when Method==options;Method==get;Method==head;Method==delete;Method==trace ->
	case http:request(Method, {Url, Headers}, [], Options) of
		{ok, saved_to_file} ->
			#http_resp{};
		{ok, {{_,RspStatus,_}, RspHeaders, RspBody}} -> 
		    #http_resp{status=RspStatus, headers=RspHeaders, body=RspBody, cookies=get_cookies(RspHeaders)};
		{error, Reason} -> 
		    exit({?MODULE, ?LINE, Reason})
	end;
	
request(Method, Url, Headers, Body, Options) 
 when Method == post; Method == put ->
    ContentType = proplists:get_value("Content-Type", Headers, "text/html"),
	case http:request(Method, {Url, Headers, ContentType, Body}, [], Options) of
		{ok, saved_to_file} ->
			#http_resp{};
		{ok, {{_,RspStatus,_}, RspHeaders, RspBody}} -> 
		    #http_resp{status=RspStatus, headers=RspHeaders, body=RspBody, cookies=get_cookies(RspHeaders)};
		{error, Reason} -> 
		    exit({?MODULE, ?LINE, Reason})
	end.

get_cookies(Headers) ->
    case proplists:get_all_values("set-cookie", Headers) of
        [] -> 
            [];
        Values ->
            dict:to_list(lists:foldl(
                fun(CookieString, Dict) ->
                     Fields =
                        [begin
                            case string:tokens(Field, "=") of
                                [Key] ->
                                    {string:strip(Key), ""};
                                [Key, Val] ->
                                    {string:strip(Key), string:strip(Val)};
                                [Key | Vals] ->
                                    {string:strip(Key), string:strip(string:join(Vals, "="))}
                            end
                        end || Field <- string:tokens(CookieString, ";")],
                     [{Name,_}|_] = Fields,
                     dict:store(Name, {cookie, CookieString, Fields}, Dict)
                end, dict:new(), Values))
    end.