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
-export([request/4]).

-define(HEADERS, [
    {"Accept","text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"},
    {"Accept-Language","en-us,en;q=0.5"},
    {"Accept-Charset","ISO-8859-1,utf-8;q=0.7,*;q=0.7"},
    {"Connection","keep-alive"},
    {"User-Agent","Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.5; en-US; rv:1.9.0.5) Gecko/2008120121 Firefox/3.0.5"}
]).

request(Method, Url, [], Body) -> request(Method, Url, ?HEADERS, Body);

request(Method, Url, Headers, []) 
 when Method==options;Method==get;Method==head;Method==delete;Method==trace ->
	case http:request(Method, {Url, Headers}, [], []) of
		{ok, {{_,RspStatus,_}, RspHeaders, RspBody}} -> {http_response, RspStatus, RspHeaders, RspBody};
		{error, Reason} -> exit({?MODULE, ?LINE, Reason})
	end;
	
request(Method, Url, Headers, Body) 
 when Method == post; Method == put ->
    ContentType = proplists:get_value("Content-Type", Headers, "text/html"),
	case http:request(Method, {Url, Headers, ContentType, Body}, [], []) of
		{ok, {{_,RspStatus,_}, RspHeaders, RspBody}} -> {http_response, RspStatus, RspHeaders, RspBody};
		{error, Reason} -> exit({?MODULE, ?LINE, Reason})
	end.