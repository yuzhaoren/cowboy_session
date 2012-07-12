-module(session_SUITE).
% Thanks to LYSE!
-include_lib("common_test/include/ct.hrl").
-export([all/0]).
-export([init_per_suite/1,
	 end_per_suite/1]).
-export([all_go/1,
	 no_session/1,
	 create_session/1,
	 check_session/1,
	 update_session/1,
	 delete_session/1
	]).

all() ->
    [all_go,
     no_session,
     create_session,
     check_session,
     update_session,
     delete_session
].

init_per_suite(Config) ->
    Port = 10000,
    ok = application:start(gproc),
    ok = application:start(ossp_uuid),
    ok = application:start(cowboy),
    ok = application:start(cowboy_session),
    Dispatch = [{'_', [{'_', session_SUITE_handler, ?config(data_dir, Config)}]}],
    cowboy:start_listener(test_listener, 5,
			  cowboy_tcp_transport, [{port, Port}],
			  cowboy_http_protocol, [{dispatch, Dispatch}]),
    ok = application:start(inets),
    [{url, "http://127.0.0.1:"++integer_to_list(Port)++"/"}|Config].

end_per_suite(Config) ->
    application:stop(cowboy),
    Config.

all_go(Config) ->
    Url = ?config(url, Config),
    {ok, {{_, 404, _},_,_}} = httpc:request(Url ++ "404"),
    Config.

no_session(Config) ->
    Url = ?config(url, Config),
    {ok, {{_, 200, _},Headers,_}} = httpc:request(Url ++ "no_session"),
    undefined = proplists:get_value("cookie", Headers),
    Config.

create_session(Config) ->
    Url = ?config(url, Config),
    % Create session
    {ok, {{_, 200, _}, Headers, _}} = httpc:request(Url ++ "create_session"),
    <<"_session=",_SessionKey:36/binary, _/binary>>=Cookie = list_to_binary(proplists:get_value("set-cookie", Headers)),
    {save_config, [{cookie,binary_to_list(Cookie)}]}.

check_session(Config) ->
    {_, SavedConfig} = ?config(saved_config, Config),
    Url = ?config(url, Config),
    Cookie = proplists:get_value(cookie, SavedConfig),
    {ok, {{_, 200, _}, _, Body}} = http(Url ++ "check_session", [{"cookie", Cookie}]),
    Session = list_to_pid(Body),
    true = is_process_alive(Session),
    {save_config, [{session,Session}|SavedConfig]}.

update_session(Config) ->
    {_,SavedConfig} = ?config(saved_config, Config),
    Url = ?config(url, Config),
    Cookie = proplists:get_value(cookie, SavedConfig),
    Session = proplists:get_value(session, SavedConfig),
    Random = ossp_uuid:make(v4, text),
    {ok, {{_, 200, _}, _, _}} = http(Url ++ "update_session/"++binary_to_list(Random), [{"cookie", Cookie}]),
    Random = cowboy_session_server:command(Session, return_random),
    {save_config, SavedConfig}.

delete_session(Config) ->
    {_,SavedConfig} = ?config(saved_config, Config),
    Url = ?config(url, Config),
    Cookie = proplists:get_value(cookie, SavedConfig),
    Session = proplists:get_value(session, SavedConfig),
    {ok, {{_, 200, _}, Headers, _}} = http(Url ++ "delete_session", [{"cookie", Cookie}]),
    <<"_session=deleted", _/binary>> = list_to_binary(proplists:get_value("set-cookie", Headers)),
    false = is_process_alive(Session).

% internal
http(Url, Headers) ->
    httpc:request(get, {Url, Headers}, [], []).
