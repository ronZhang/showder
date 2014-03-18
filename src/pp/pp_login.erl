%% @author Ron
%% @doc 登录消息处理.


-module(pp_login).
-include("simple_pb.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([test/2]).

test(Socket,Person)->
	Name=Person#person.name,
	Bin=pt_10:write(10000, Name),
	lib_send:send(Socket, Bin)
.




%% ====================================================================
%% Internal functions
%% ====================================================================


