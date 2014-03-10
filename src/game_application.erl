%% @author ron
%% @doc @todo Add description to game_application.


-module(game_application).
-behaviour(application).
-export([start/0,start/2,stop/1,start/3]).

start() -> 
	[Ip,Port,SerId]=init:get_plain_arguments(),
	start(Ip,Port,SerId).


%%行为模式回调
start(_TYPE,Argum) ->
	[Ip,Port,SerId]=Argum,
	case start(Ip,Port,SerId) of
		{ok,Pid} when is_pid(Pid) ->
			log4erl:info("game_sup started"),
			{ok,Pid};
		Error -> {error,Error}
	end.



start(Ip,Port,SerId) -> 
	%%启动网络
	start_mod(Ip,Port,SerId).

stop(State) ->
    ok.

start_mod(Ip,Port,SerId)->
	%%启动顶层监督
    RootSub= game_sup:start_link(),
	%%启动socket监听
	start_tcp_sup(Port),
	%%启动客户端监听者
	start_client_sup(),
	
	RootSub
.


%%开启客户端监控树
start_client_sup() ->
	log4erl:info("client_sup statting"),
    {ok,_} = supervisor:start_child(
               game_sup,
               {game_client_sup,
                {game_client_sup, start_link,[]},
                transient, infinity, supervisor, [game_client_sup]}),
	log4erl:info("client_sup started"),
    ok.

%%开启tcp listener监控树
start_tcp_sup(Port) ->
	log4erl:info("sd_tcp_listener_sup statting"),
    {ok,_} = supervisor:start_child(
               game_sup,
               {game_tcp_listener_sup,
                {game_tcp_listener_sup, start_link, [Port]},
                transient, infinity, supervisor, [game_tcp_listener_sup]}),
	log4erl:info("sd_tcp_listener_sup started"),
    ok.
