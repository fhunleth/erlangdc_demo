-module(ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    hwinterface:register_pushbutton_listener(self()),
    {ok, Req, 0}.

websocket_handle({text, Msg}, Req, State) ->
    ParsedMessage = jsx:decode(Msg),
    io:format("Got message: ~p~n", [ParsedMessage]),
    handle_request(ParsedMessage),
    {ok, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

handle_request([<<"setYellow">>, Value]) ->
    hwinterface:set_led(yellow, Value);
handle_request([<<"setRed">>, Value]) ->
    hwinterface:set_led(red, Value);
handle_request([<<"setGreen">>, Value]) ->
    hwinterface:set_led(green, Value).

websocket_info({pushbutton, rising}, Req, State) ->
    Response = jsx:encode([<<"buttonStatus">>, 1]),
    {reply, {text, Response}, Req, State};
websocket_info({pushbutton, falling}, Req, State) ->
    Response = jsx:encode([<<"buttonStatus">>, 0]),
    {reply, {text, Response}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.
