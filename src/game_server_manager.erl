%% @author Ron
%% @doc @todo Add description to game_tcp_listener.
%%socket监听进程

-module(game_server_manager).
-record(state, {servers}).
-record(server, {serverId,serverName,userNum}).
-behaviour(gen_server).
-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).

%% ====================================================================
%% behavior functions
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2, 
	terminate/2, 
	code_change/3]).
%% ====================================================================


start_link() ->
    gen_server:start_link(?MODULE, {}, []).

init(_Any) ->
	 {ok, #state{servers=[]}}.


%%游戏服务器启动通知
handle_call({server_start,ServerId,ServerName}, _From, State) ->
	OldSservers=State#state.servers,
	
	NewState=#state{servers=[{ServerId,ServerName}|OldSservers]},
	_From ! ok,
    {reply,NewState,NewState};

%%收到游戏服务器关闭通知
handle_call({server_closed,ServerId,ServerName}, _From, State) ->
	OldSservers=State#state.servers,
	NewServers=lists:delete({ServerId,ServerName},OldSservers),
	NewState=#state{servers=NewServers},
	_From ! ok,
    {reply,NewState,NewState}.



%%服务器人数增加
handle_cast({user_connected,ServerId}, State) ->
	Servers=State#state.servers,
    {noreply, State};



%%服务器人数减少
handle_cast({user_released,ServerId}, State) ->
    {noreply, State};



handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%更新服务器人数变化
update_server_num(ServerId,[])->[];

update_server_num(ServerId,[{ServerId,ServerName,ServerNum}|T])->
	[{ServerId,ServerName,ServerNum+1}|T];

update_server_num(ServerId,[H|T])->
	[H|update_server_num(ServerId,T)].
