%% @author Ron
%% @doc @todo logger 监督进程.
-module(logger_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).


%% ====================================================================
%% Internal functions
%% ====================================================================
start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 10, 10},
          [{game_client_reader, {game_client_reader,start_link,[]},
            temporary, brutal_kill, worker, [game_client_reader]}]}}.
