%% @author Ron
%% @doc @todo Add description to logger.
%%日志进程模块，采用异步记录


-module(logger).
-behavior(gen_server).
%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%% ====================================================================
%% Internal functions
%% ====================================================================


