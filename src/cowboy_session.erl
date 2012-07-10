-module(cowboy_session).
-export([create_session/2,
	 get_session/2,
	 get_or_create/2,
	 delete_session/2,
	 on_request/2,
	 on_response/2
	]).
-include_lib("cowboy/include/http.hrl").

% Workflow one, use the request to piggyback the session
on_request(Req, Handler) ->
    {Session, Req1} = get_or_create(Req, Handler),
    Req1#http_req{ meta = [{cowboy_session, Session}|Req1#http_req.meta]}.

on_response(Req, Handler) ->
    {Pid, Req1} = from_request(Req),
    CookieName = Handler:cookie_name(),
    HandlerState = cowboy_session_server:handler_state(Pid),
    Options = Handler:cookie_options(HandlerState),
    ResponseSession = cowboy_session_server:session_id(Pid),
    {ok, Req2} = cowboy_http_req:set_resp_cookie(CookieName, ResponseSession, Options, Req1),
    Req2.

from_request(Req) ->
    cowboy_http_req:meta(cowboy_session, Req).

% Workflow two, manual
delete_session(Req, Handler) ->
    CookieName = Handler:cookie_name(),
    case get_cookie(CookieName, Req) of
	undefined ->
	    Req;
	_B ->
	    {Session, Req1} = get_session(Req, Handler),
	    Handler:stop(Session),
	    {ok, Req1} = cowboy_http_req:set_resp_cookie(CookieName, <<"deleted">>,
							 [{set_age, 0},
							  {local_time, {{1970,1,1},{0,0,0}}}], Req),
	    Req1
    end.

get_or_create(Req, Handler) ->
    case get_session(Req, Handler) of
	undefined ->
	    create_session(Req, Handler);
	{_, _}=Res ->
	    Res
    end.

create_session(Req, Handler) ->
    CookieName = Handler:cookie_name(),
    Session = case get_cookie(CookieName, Req) of
		  undefined ->
		      undefined;
		  B ->
		      B
	      end,
    NewSession = case Session of
		     undefined ->
			 Handler:generate();
		     _ ->
			 Handler:validate(Session)
		 end,
    {ok, Pid} = supervisor:start_child(cowboy_session_server_sup, [NewSession, Handler:session_name(NewSession)]),
    HandlerState = cowboy_session_server:handler_state(Pid),
    Options = Handler:cookie_options(HandlerState),
    ResponseSession = cowboy_session_server:session_id(Pid),
    {ok, Req1} = cowboy_http_req:set_resp_cookie(CookieName, ResponseSession, Options, Req),
    {Pid, Req1}.

get_session(Req, Handler) ->
    case get_cookie(Handler:cookie_name(), Req) of
	undefined ->
	    no_session;
	B ->
	    SessionName = Handler:session_name(B),
	    case gproc:lookup_local_name({cowboy_session, SessionName}) of
		undefined ->
		    no_session;
		Pid1 ->
		    cowboy_session_server:touch(Pid1),
		    {Pid1, Req}
	    end    
    end.

get_cookie(CookieName, Req) ->
    case cowboy_http_req:cookie(CookieName, Req) of
	{undefined, _} ->
	    undefined;
	{B, _} when is_binary(B) ->
	    B
    end.
