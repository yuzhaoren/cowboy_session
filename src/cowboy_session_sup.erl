-module(cowboy_session_sup).
-behaviour(supervisor).
%% API
-export([start_link/0, start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(I, StartFunc, Restart), {I, StartFunc,
				       temporary,
				       5000,
				       Restart, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    start_link(cowboy_session_default_handler).

start_link(Handler) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Handler]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================


init([Handler]) ->
    {ok, {{simple_one_for_one, 0, 1},
	  [?CHILD(cowboy_session_server,
		  {cowboy_session_server, start_link, [Handler]},
		  transient)
	  ]}}.
