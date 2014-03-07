%% @author ron
%% @docAdd description to pp_10.
%%测试模块

-module(pp_10).
-include(simple_db.hrl).
%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Internal functions
%% ====================================================================
read(10000, Binary) ->
	Person=simple_pb:simple_pb:decode_person(Binary),	
    {ok,person,Person};


%%组装消息格式
write(10100,Data)->
	{ok,pt:pack(10100,Data)}.


	


