%% @author ron
%% @doc @todo Add description to game_application.


-module(game_application).
-behaviour(application).
-export([start/0,stop/1,start/3]).

start() -> 
	[Ip,Port,SerId]=init:get_plain_arguments(),
	start(Ip,Port,SerId).


start(Ip,Port,SerId) -> 
	%%启动网络
	start_mod(Ip,Port,SerId).

stop(State) ->
    ok.

start_mod(Ip,Port,SerId)->
	%%启动顶层监督
    game_sup:start_link(),
	%%启动socket监听
	start_tcp_sup(Port),
	%%启动客户端监听者
	start_client_sup()
.

%%开启客户端监控树
start_client_sup() ->
    {ok,_} = supervisor:start_child(
               sd_sup,
               {sd_tcp_client_sup,
                {sd_tcp_client_sup, start_link,[]},
                transient, infinity, supervisor, [sd_tcp_client_sup]}),
    ok.

%%开启tcp listener监控树
start_tcp_sup(Port) ->
    {ok,_} = supervisor:start_child(
               sd_sup,
               {sd_tcp_listener_sup,
                {sd_tcp_listener_sup, start_link, [Port]},
                transient, infinity, supervisor, [sd_tcp_listener_sup]}),
    ok.
