%% @author Administrator
%% @doc @todo Add description to game-tcp_acceptor_sup.


-module(game_tcp_acceptor_sup).
-behaviour(supervisor).


%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, init/1]).

start_link()->
	supervisor:start_link({local,?MODULE}, ?MODULE,[]).



init([]) ->
    {ok, {{simple_one_for_one, 10, 10},
          [{game_tcp_acceptor, {game_tcp_acceptor, start_link, []},
            transient, brutal_kill, worker, [game_tcp_acceptor]}]}}.


