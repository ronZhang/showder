%%%-----------------------------------
%%% @Module  : sd_tcp_client_sup
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2010.06.01
%%% @Description: socket链接监督者
%%%-----------------------------------
-module(game_client_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 10, 10},
          [{game_client_reader, {game_client_reader,start_link,[]},
            temporary, brutal_kill, worker, [game_client_reader]}]}}.
