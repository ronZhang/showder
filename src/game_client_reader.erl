%%%-----------------------------------
%%% @Module  : sd_reader
%%% @Author  : xyao
%%% @Email   : jiexiaowen@gmail.com
%%% @Created : 2010.06.1
%%% @Description: 读取客户端
%%%-----------------------------------
-module(game_client_reader).
-export([start_link/0, init/0]).
-include("common.hrl").
-include("record.hrl").
-define(TCP_TIMEOUT, 1000). % 解析协议超时时间
-define(HEART_TIMEOUT, 60000). % 心跳包超时时间
-define(HEART_TIMEOUT_TIME, 0). % 心跳包超时次数
-define(HEADER_LENGTH, 4). % 消息头长度

%%记录客户端进程
-record(client, {
            player = none,
            login  = 0,
            accid  = 0,
            accname = none,
            timeout = 0 % 超时次数
     }).

start_link() ->
    {ok, proc_lib:spawn_link(?MODULE, init, [])}.

%%gen_server init
%%Host:主机IP
%%Port:端口
init() ->
    process_flag(trap_exit, true),
    Client = #client{
                player = none,
                login  = 0,
                accid  = 0,
                accname = none,
                timeout = 0 
            },
    receive
        {go,Socket}->
            login_parse_packet(Socket, Client)
    end.

%%接收来自客户端的数据 - 先处理登陆
%%Socket：socket id
%%Client: client记录
login_parse_packet(Socket,Client) ->
    Ref = async_recv(Socket, ?HEADER_LENGTH, ?HEART_TIMEOUT),
    receive
        %%flash安全沙箱
        {inet_async, Socket, Ref, {ok, ?FL_POLICY_REQ}} ->
            Len = 23 - ?HEADER_LENGTH,
            async_recv(Socket, Len, ?TCP_TIMEOUT),
            lib_send:send_one(Socket, ?FL_POLICY_FILE),
            gen_tcp:close(Socket);

        %%消息处理
        {inet_async, Socket, Ref, {ok, <<Len:16, Cmd:16>>}} ->
            BodyLen = Len - ?HEADER_LENGTH,
            case BodyLen > 0 of
                true ->
                    Ref1 = async_recv(Socket,BodyLen,?TCP_TIMEOUT),
                    receive
                       {inet_async, Socket, Ref1, {ok, Binary}} ->
						   %%调用相应的消息模块封装消息
                            case routing(Cmd, Binary) of
								{ok,_,Data} ->
									%%调用消息处理模块
									case msg_handle(Cmd, Data) of
										%%报错断开连接
										{error,Reason} -> login_lost(Socket, Client, 0,Reason);
										_ ->  login_parse_packet(Socket, Client)
									end;
                               	 Other ->
                                    login_lost(Socket, Client, 0, Other)
                            end;
                        Other ->
                            login_lost(Socket, Client, 0, Other)
                    end;
				%%内容长度为空
                false -> login_lost(Socket, Client, 0, "login fail")
            end;

        %%超时处理
        {inet_async, Socket, Ref, {error,timeout}} ->
            case Client#client.timeout >= ?HEART_TIMEOUT_TIME of
                true ->
                    login_lost(Socket, Client, 0, {error,timeout});
                false ->
                    login_parse_packet(Socket, Client#client {timeout = Client#client.timeout+1})
            end;

        %%用户断开连接或出错
        Other ->
            login_lost(Socket, Client, 0, Other)
    end.

%%接收来自客户端的数据 - 登陆后进入游戏逻辑
%%Socket：socket id
%%Client: client记录
do_parse_packet(Socket, Client) ->
    Ref = async_recv(Socket, ?HEADER_LENGTH, ?HEART_TIMEOUT),
    receive
        {inet_async, Socket, Ref, {ok, <<Len:16, Cmd:16>>}} ->
            BodyLen = Len - ?HEADER_LENGTH,
            case BodyLen > 0 of
                true ->
                    Ref1 = async_recv(Socket, BodyLen, ?TCP_TIMEOUT),
                    receive
                       {inet_async, Socket, Ref1, {ok, Binary}} ->
                            case routing(Cmd, Binary) of
                                %%这里是处理游戏逻辑
                                {ok, Data} ->
                                    case catch gen:call(Client#client.player, '$gen_call', {'SOCKET_EVENT', Cmd, Data}) of
                                        {ok,_Res} ->
                                            do_parse_packet(Socket, Client);
                                        {'EXIT',Reason} ->
                                             do_lost(Socket, Client, Cmd, Reason)
                                    end;
                                Other ->
                                    do_lost(Socket, Client, Cmd, Other)
                            end;
                         Other ->
                            do_lost(Socket, Client, Cmd, Other)
                    end;
                false ->
                    case routing(Cmd, <<>>) of
                        %%这里是处理游戏逻辑
                        {ok, Data} ->
                            case catch gen:call(Client#client.player, '$gen_call', {'SOCKET_EVENT', Cmd, Data}, 3000) of
                                {ok,_Res} ->
                                    do_parse_packet(Socket, Client);
                                {'EXIT',Reason} ->
                                    do_lost(Socket, Client, Cmd, Reason)
                            end;
                        Other ->
                            do_lost(Socket, Client, Cmd, Other)
                    end
            end;

        %%超时处理
        {inet_async, Socket, Ref, {error,timeout}} ->
            case Client#client.timeout >= ?HEART_TIMEOUT_TIME of
                true ->
                    do_lost(Socket, Client, 0, {error,timeout});
                false ->
                    do_parse_packet(Socket, Client#client {timeout = Client#client.timeout+1})            
            end;
            
        %%用户断开连接或出错
        Other ->
            do_lost(Socket, Client, 0, Other)
    end.

%%断开连接
login_lost(Socket, _Client, _Cmd, Reason) ->
    gen_tcp:close(Socket),
    exit({unexpected_message, Reason}).

%%退出游戏
do_lost(_Socket, Client, _Cmd, Reason) ->
    mod_login:logout(Client#client.player),
    exit({unexpected_message, Reason}).

%%路由
%%组成如:pt_10:read
routing(Cmd, Binary) ->
    %%取前面二位区分功能类型
    [H1, H2, _, _, _] = integer_to_list(Cmd),
    Module = list_to_atom("pp_"++[H1,H2]),
    Module:read(Cmd, Binary).

%% routing(Cmd, Binary) ->
%%     %%取前面二位区分功能类型
%%     [H1, H2, _, _, _] = integer_to_list(Cmd),
%%     Module = list_to_atom("pt_"++[H1,H2]),
%%     Module:read(Cmd, Binary).


%%处理消息调用
%% msg_handle(Cmd,[]) ->
%% %% 	 [ModuleName|T] =Data,
%% %% 	 Module=list_to_atom("handle_"++[ModuleName]),
%% %% 	 Module:handle(Cmd,T)
%%  io:format("receive msg ~p~n",[Cmd]);

msg_handle(Cmd,Data) ->
%% 	 [ModuleName|T] =Data,
%% 	 Module=list_to_atom("handle_"++[ModuleName]),
%% 	 Module:handle(Cmd,T)
 io:format("receive msg ~p~p~n",[Cmd,Data]). 

%% 接受信息
async_recv(Sock, Length, Timeout) when is_port(Sock) ->
    case prim_inet:async_recv(Sock, Length, Timeout) of
        {error, Reason} -> throw({Reason});
        {ok, Res}       -> Res;
        Res             -> Res
    end.
