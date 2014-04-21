# Sessions for Cowboy
Provides a session API for the Cowboy HTTP server.

# 我使用cowboy_session对页面进行登陆session校验的过程

1. 在项目中添加依赖
   
{'cowboy_session', ".*", { git, "git://github.com/dparnell/cowboy_session.git", "master"} }

2. 项目app.erl中启动cowboy_seesion (gproc,ossp_uuid,cowboy也要启动)

start(_StartType, _StartArgs) ->
    ok = application:start(gproc),
    ok = application:start(ossp_uuid),
    ok = application:start(cowboy),
    ok = application:start(cowboy_session).

3. 在自己项目里创建一个session handler模块

-module(http_session).
-export([cookie_name/0, cookie_options/1, session_name/1, generate/0, stop/2, validate/1,
         timeout/1, handle_expired/2, touch/2, handle/3, init/2]).

validate(_Session) ->
   ?MODULE:generate().

init(_Session, _SessionName) ->
    dict:new().

timeout(_Session) ->
    1000 * 60 * 30.

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

cookie_name() ->
    <<"_session">>.

cookie_options(_HandlerState) ->
    [{path, <<"/">>}].

generate() ->
    ossp_uuid:make(v4, text).

session_name(Session) ->
    Session.

-spec stop(binary(), any()) -> any().
stop(_Session, _State) ->
    ok.

-spec touch(any(), any()) -> any().
touch(_Session, State) ->
    State.

-spec handle_expired(binary(), any()) -> any().
handle_expired(_Session,HandlerState) ->
    HandlerState.


4.服务器端接收到请求后,根据Req中的cookie进行session校验

{Session, Req0} = cowboy_session:get_session(Req, ?HANDLER), 
   case Session of
    undefined -> 失效则跳转到登陆页面
    Pid       -> 有效则处理请求并返回
   end

5. 登陆成功时,服务器端创建session,将session返回给浏览器端(存在response的cookie中)

   {Session, Req1} = cowboy_session:create_session(Req, ?HANDLER), 
   
6. 登出操作,删除session,跳转到登陆页面

    Req1 = cowboy_session:delete_session(Req, ?HANDLER),


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
