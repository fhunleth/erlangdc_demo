%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(erlangdc_demo_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.
start(_Type, _Args) ->
    % Bring up the ethernet interface to a hardcoded IP
    % address for now.
    os:cmd("/sbin/ip link set eth0 up"),
    os:cmd("/sbin/ip addr add 192.168.1.40/24 dev eth0"),
    os:cmd("/sbin/ip route add default via 192.168.1.1"),

    Dispatch = cowboy_router:compile([
				      {'_', [
					     {"/", cowboy_static, {priv_file, erlangdc_demo, "index.html"}},
					     {"/websocket", ws_handler, []},
					     {"/static/[...]", cowboy_static, {priv_dir, erlangdc_demo, "static"}}
					    ]}
				     ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8000}],
				[{env, [{dispatch, Dispatch}]}]),
    erlangdc_demo_sup:start_link().

stop(_State) ->
    ok.
