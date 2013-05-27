# Sessions for Cowboy
Provides a session API for the Cowboy HTTP server.

# How do I use this thing?

First you need a session handler module:

````erlang
-module(my_session_handler).
-extends(cowboy_session_default_handler).
-export([validate/1, init/2, handle/3]).

validate(_Session) ->
   ?MODULE:generate().

init(_Session, _SessionName) ->
    dict:new().

handle({get, Key}, _Session, State) ->
    Value = case dict:find(Key, State) of
                error -> undefined;
                {ok, V} -> V
            end,

    {Value, State};

handle({set, Key, Value}, _Session, State) ->
    {ok, dict:store(Key, Value, State) };

handle(dict, _Session, State) ->
    {State, State}.
````

then you can use the session from a cowboy HTTP handler as follows:

````erlang
    %% get the session
    {Session, Req2} = cowboy_session:get_or_create(Req, my_session_handler),
    %% set the user id
    ok = cowboy_session_server:command(Session, {set, user_id, Id}),
    %% later get the session id later
    UserId = cowboy_session_server:command(Session, {get, user_id}),
````

This is how I use it, but you're free to store your session in any way you like.

*NOTE: I've been unable to get it to with under R16*

# Tests
`make test`

# Todo
* Implement the signed stateless session
