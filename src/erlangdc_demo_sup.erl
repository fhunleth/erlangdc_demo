%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(erlangdc_demo_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

init([]) ->
    {ok, { {one_for_one, 5, 10},
	   [?CHILD(hwinterface, hwinterface, worker, [])]} }.
