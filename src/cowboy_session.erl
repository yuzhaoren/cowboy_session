-module(cowboy_session).
-export([create_session/2,
         get_session/2,
         get_or_create/2,
         delete_session/2,
         on_request/2,
         on_response/2
        ]).

                                                % Workflow one, use the request to piggyback the session
on_request(Req, Handler) ->
    {Session, Req1} = get_or_create(Req, Handler),
    cowboy_req:set_meta(cowboy_session, Session, Req1).

on_response(Req, Handler) ->
    {Pid, Req1} = from_request(Req),
    CookieName = Handler:cookie_name(),
    HandlerState = cowboy_session_server:handler_state(Pid),
    Options = Handler:cookie_options(HandlerState),
    ResponseSession = cowboy_session_server:session_id(Pid),
    cowboy_req:set_resp_cookie(CookieName, ResponseSession, Options, Req1).    

from_request(Req) ->
    cowboy_req:meta(cowboy_session, Req).

                                                % Workflow two, manual
delete_session(Req, Handler) ->
    case get_session(Req, Handler) of
        undefined ->
            Req;
        {Session, Req1} ->
            HandlerState = cowboy_session_server:handler_state(Session),
            Options = Handler:cookie_options(HandlerState),
            cowboy_session_server:stop(Session),
            cowboy_req:set_resp_cookie(Handler:cookie_name(), <<"deleted">>,
                                       [{set_age, 0},
                                        {local_time, {{1970,1,1},{0,0,0}}}|Options], Req1)
    end.

get_or_create(Req, Handler) ->
    case get_session(Req, Handler) of
        undefined ->
            create_session(Req, Handler);
        {_, _} = Res ->
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
    {ok, Pid} = supervisor:start_child(cowboy_session_server_sup, [Handler, NewSession, Handler:session_name(NewSession)]),
    HandlerState = cowboy_session_server:handler_state(Pid),
    Options = Handler:cookie_options(HandlerState),
    ResponseSession = cowboy_session_server:session_id(Pid),
    Req1 = cowboy_req:set_resp_cookie(CookieName, ResponseSession, Options, Req),
    {Pid, Req1}.

get_session(Req, Handler) ->
    case get_cookie(Handler:cookie_name(), Req) of
        undefined ->
            undefined;
        B ->
            SessionName = Handler:session_name(B),
            case gproc:lookup_local_name({cowboy_session, SessionName}) of
                undefined ->
                    undefined;
                Pid1 ->
                    cowboy_session_server:touch(Pid1),
                    {Pid1, Req}
            end
    end.

get_cookie(CookieName, Req) ->
    case cowboy_req:cookie(CookieName, Req) of
        {undefined, _} ->
            undefined;
        {B, _} when is_binary(B) ->
            B
    end.
