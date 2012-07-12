-module(session_SUITE_handler).
-export([init/3, handle/2, terminate/2]).
-define(HANDLER, session_SUITE_session_handler).
-record(state, {dir}).
init({tcp, http}, Req, Dir) ->    
    {ok, Req, #state{dir=Dir}}.

handle(Req, State) ->
    {Method, Req1} = cowboy_http_req:method(Req),
    {Path, Req2} = cowboy_http_req:path(Req1),
    handle(Req2, Method, Path, State).

terminate(_Req, _State) ->
    ok.

% Internal
handle(Req, 'GET', [<<"404">>], State) ->
    {ok, Req1} = cowboy_http_req:reply(404, [], <<>>, Req),
    {ok, Req1, State};
handle(Req, 'GET', [<<"no_session">>], State) ->
    {ok, Req1} = cowboy_http_req:reply(200, [], <<>>, Req),
    {ok, Req1, State};
handle(Req, 'GET', [<<"check_session">>], State) ->
    {Session, Req0} = cowboy_session:get_session(Req, ?HANDLER),
    {ok, Req1} = cowboy_http_req:reply(200, [], list_to_binary(pid_to_list(Session)), Req0),
    {ok, Req1, State};
handle(Req, 'GET', [<<"update_session">>, Random], State) ->
    {Session, Req0} = cowboy_session:get_session(Req, ?HANDLER),
    ok = cowboy_session_server:command(Session, {random, Random}),
    {ok, Req1} = cowboy_http_req:reply(200, [], <<>>, Req0),
    {ok, Req1, State};
handle(Req, 'GET', [<<"delete_session">>], State) ->
    Req1 = cowboy_session:delete_session(Req, ?HANDLER),
    {ok, Req2} = cowboy_http_req:reply(200, [], <<>>, Req1),
    {ok, Req2, State};
handle(Req, 'GET', [<<"create_session">>], State) ->
    {_Session, Req1} = cowboy_session:create_session(Req, ?HANDLER),
    {ok, Req2} = cowboy_http_req:reply(200, [], <<>>, Req1),
    {ok, Req2, State}.
