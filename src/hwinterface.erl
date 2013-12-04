%%%-------------------------------------------------------------------
%%% @author Frank Hunleth <fhunleth@troodon-software.com>
%%% @copyright (C) 2013, Frank Hunleth
%%% @doc
%%% Hardware interface to the LED and switch demo on the
%%% Beaglebone.
%%% @end
%%% Created :  4 Dec 2013 by Frank Hunleth <fhunleth@troodon-software.com>
%%%-------------------------------------------------------------------
-module(hwinterface).

-behaviour(gen_server).

%% API
-export([start_link/0, set_led/2, get_pushbutton/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(RED_LED_PIN, 44).
-define(GREEN_LED_PIN, 47).
-define(YELLOW_LED_PIN, 23).
-define(PUSH_BUTTON_PIN, 46).

%%%===================================================================
%%% API
%%%===================================================================

set_led(Color, Value) ->
    gen_server:cast(?SERVER, {set_led, Color, Value}).

get_pushbutton() ->
    gen_server:call(?SERVER, get_pushbutton).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, _} = gpio:start_link({?PUSH_BUTTON_PIN, input}),
    {ok, _} = gpio:start_link({?RED_LED_PIN, output}),
    {ok, _} = gpio:start_link({?YELLOW_LED_PIN, output}),
    {ok, _} = gpio:start_link({?GREEN_LED_PIN, output}),
    {ok, undefined}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(get_pushbutton, _From, State) ->
    Value = gpio:read(?PUSH_BUTTON_PIN),
    {reply, Value, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({set_led, Color, Value}, State) ->
    gpio:write(color_to_pin(Color), Value),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
color_to_pin(red) -> ?RED_LED_PIN;
color_to_pin(yellow) -> ?YELLOW_LED_PIN;
color_to_pin(green) -> ?GREEN_LED_PIN.
