-module(session_SUITE_session_handler).
-extends(cowboy_session_default_handler).
-export([validate/1, init/2, handle/3, info/3]).

-record(state, {random}).

validate(_Session) ->
    ?MODULE:generate().

init(_Session, _SessionName) ->
    #state{}.

handle({random, Random}, _Session, State) ->
    error_logger:info_msg("Random is ~p", [State]),
    {ok, State#state{random=Random}};
handle(return_random, _Session, #state{random=Random}=State) ->
    {Random, State}.

info(_Info, _Session, State) ->
    {ok, State}.
