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
    erlang:start_timer(1000, self(), check_button),
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

websocket_info({timeout, _Ref, check_button}, Req, State) ->
    %% Check if the button state changed
    erlang:start_timer(1000, self(), check_button),
    NewState = hwinterface:get_pushbutton(),
    case NewState == State of
	true -> {ok, Req, State};
	false ->
	    Response = jsx:encode([<<"buttonStatus">>, NewState]),
	    {reply, {text, Response}, Req, NewState}
    end;
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.
