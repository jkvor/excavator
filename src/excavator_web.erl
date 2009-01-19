-module(excavator_web).
-export([
	request/4
]).

request(Method, Url, Headers, []) 
 when Method==options;Method==get;Method==head;Method==delete;Method==trace ->
	case http:request(Method, {Url, Headers}, [], []) of
		{ok, Response} -> Response;
		{error, Reason} -> exit(Reason)
	end;
	
request(Method, Url, Headers, Body) 
 when Method == post; Method == put ->
	case http:request(Method, {Url, Headers, "text/html", Body}, [], []) of
		{ok, Response} -> Response;
		{error, Reason} -> exit(Reason)
	end.