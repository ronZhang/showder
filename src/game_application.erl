%% @author ron
%% @doc @todo Add description to game_application.


-module(game_application).
%% -behaviour(application).
-export([start/0,stop/1,start/3]).

%% ====================================================================
%% API functions
%% ====================================================================



%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% start/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/apps/kernel/application.html#Module:start-2">application:start/2</a>
%%游戏applicaton启动
%% ====================================================================
start() -> 
	[Ip,Port,SerId]=init:get_plain_arguments(),
	start(Ip,Port,SerId).


start(Ip,Port,SerId) -> 
	%%启动其他模块
	%%启动网络
	start_mod(Ip,Port,SerId).

%% stop/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/apps/kernel/application.html#Module:stop-1">application:stop/1</a>
-spec stop(State :: term()) ->  Any :: term().
%% ====================================================================
stop(State) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================
start_mod(Ip,Port,SerId)->
    %%启动顶层监督
    game_sup:start_link(),
%%启动socket监听
	start_disperse([Ip, Port,SerId])
.

%%启动端口监听
start_disperse([Ip, Port, Sid]) ->
    {ok,_} = supervisor:start_child(
               game_sup,
               {mod_net,
                {mod_net, start_link,[Ip, Port, Sid]},
                permanent, 10000, supervisor, [mod_net]}),
    ok.
