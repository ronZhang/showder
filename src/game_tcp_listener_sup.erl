%% @author Ron
%%socket链接监督者
%% @doc @todo Add description to game_tcp_listener_sup.


-module(game_tcp_listener_sup).
-behaviour(supervisor).
%% ====================================================================
%% API functions
-export([start_link/1]).
%% ====================================================================
-export([init/1]).

%%启动
start_link(Port) ->
	supervisor:start_link({local,?MODULE}, ?MODULE, {10,Port}).





%% ====================================================================
%% behavior functions
%% ====================================================================
init({AcceptorCount, Port}) ->
    {ok,
        {{one_for_all, 10, 10},
            [
                {
                    game_tcp_acceptor_sup,
                    {game_tcp_acceptor_sup, start_link, []},
                    transient,
                    infinity,
                    supervisor,
                    [game_tcp_acceptor_sup]
                }
%% 			,
%%                 {
%%                     sd_tcp_listener,
%%                     {sd_tcp_listener, start_link, [AcceptorCount, Port]},
%%                     transient,
%%                     100,
%%                     worker,
%%                     [sd_tcp_listener]
%%                 }
            ]
        }
    }.

