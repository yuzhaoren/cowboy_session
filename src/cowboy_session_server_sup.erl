-module(cowboy_session_server_sup).
-behaviour(supervisor).
%% API
-export([start_link/0,
	 add_session/3]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(I, StartFunc, Restart), {I, StartFunc,
				       Restart,
				       5000,
				       worker, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

add_session(Handler, Session, SessionName) ->
    supervisor:start_child(cowboy_session_server_sup, [Handler, Session, SessionName]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
    {ok, {{simple_one_for_one, 0, 1},
	  [?CHILD(cowboy_session_server,
		  {cowboy_session_server, start_link, []},
		  transient)
	  ]}}.
