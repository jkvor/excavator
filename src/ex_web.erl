-module(ex_web).
-export([
	request/4
]).

-define(HEADERS, [
	{"Accept","text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"},
	{"Accept-Language","en-us,en;q=0.5"},
	%{"Accept-Encoding","gzip,deflate"},
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
	case http:request(Method, {Url, Headers, "text/html", Body}, [], []) of
		{ok, {{_,RspStatus,_}, RspHeaders, RspBody}} -> {http_response, RspStatus, RspHeaders, RspBody};
		{error, Reason} -> exit({?MODULE, ?LINE, Reason})
	end.