%% @author ron
%% @doc 数据访问API
%%将持久层的细节封装，提供api
%%注意保持接口的稳定性，和平台独立性


-module(login_dao).
-include("record.hrl").
%%存储的表名字
-define(PlayerTable,t_player).

%% ====================================================================
%% API functions
%% ====================================================================
-export([getUser/1]).


getUser(PlayerId)->
		case dao:get(?PlayerTable,1,PlayerId) of
			{ok,Data} -> Data;
			
		end
	.
	



%% ====================================================================
%% Internal functions
%% ====================================================================


