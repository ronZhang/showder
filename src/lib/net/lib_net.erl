%% @author Ron
%% @doc @todo Add description to lib_net.


-module(lib_net).
%% ====================================================================
%% API functions
%% ====================================================================
-export([send/2]).


send(PlayerId,Bin)->
	game_tcp_sender:send(PlayerId, Bin).


%% ====================================================================
%% Internal functions
%% ====================================================================


