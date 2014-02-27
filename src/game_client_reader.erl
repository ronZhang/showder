%% @author Ron
%% @doc @todo Add description to tcp_reader.
%%socket_worker
%%socket读写进程
-module(game_client_reader).
-record(state, {}).
-behaviour(gen_server).
%% ====================================================================
%% API functions
%% ====================================================================
-export([]).





%% ====================================================================
%% Internal functions
%% ====================================================================


%%behavior function
%%%------------------------------------
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2, 
	terminate/2, 
	code_change/3]).
init([]) ->
	{ok,#state{}}.

handle_cast(_R, State) ->
    {noreply, State}.

handle_call(_R, _FROM, State) ->
    {reply, ok, State}.

handle_info(_Reason, State) ->
    {noreply, State}.

terminate(_R, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra)->
    {ok, State}.




