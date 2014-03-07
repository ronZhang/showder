%% @author Administrator
%% @doc @todo Add description to pt.


-module(pt).

%% ====================================================================
%% API functions
%% ====================================================================
-export([pack/2]).

%% 组装消息 <<总长度:2个字节，消息序号:2个字节，内容>>
pack(Cmd, Data) ->
    L = byte_size(Data) + 4,
    <<L:16, Cmd:16, Data/binary>>.

%% ====================================================================
%% Internal functions
%% ====================================================================


