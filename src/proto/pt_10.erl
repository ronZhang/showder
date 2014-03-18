%% @author ron
%% @docAdd description to pp_10.
%%测试模块

-module(pt_10).
-include("simple_pb.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/2,write/2]).

%%消息处理test
handle(10000,Binary) ->
	Person=simple_pb:decode_person(Binary),	
	pp_login:test(Person),
	ok;

handle(10001,Binary) ->
	Person=simple_pb:decode_person(Binary),	
	ok.









%%组装消息格式
write(10100,Data)->
	{ok,pt:pack(10100,Data)}.
	


