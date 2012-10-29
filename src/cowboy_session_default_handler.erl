-module(cowboy_session_default_handler).
-export([cookie_name/0, cookie_options/1, session_name/1, generate/0, stop/2, validate/1,
         timeout/1, handle_expired/2, touch/2, handle/3, init/2]).

cookie_name() ->
    <<"_session">>.

cookie_options(_HandlerState) ->
    [{path, <<"/">>}].

generate() ->
    ossp_uuid:make(v4, text).

session_name(Session) ->
    Session.

-spec timeout(binary()) -> integer().
timeout(_Session) ->
    infinity.

validate(Session) ->
    Session.

-spec init(binary(), any()) -> any().
init(_Session, _SessionName) ->
    undefined.

-spec stop(binary(), any()) -> any().
stop(_Session, _State) ->
    ok.

-spec touch(any(), any()) -> any().
touch(_Session, State) ->
    State.

-spec handle(any(), binary(), any()) -> {any(), any()}.
handle(_Command, _Session, State) ->
    {ok, State}.
-spec handle_expired(binary(), any()) -> any().
handle_expired(_Session,HandlerState) ->
    HandlerState.
