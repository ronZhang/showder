%% @author Ron
%% @doc 消息发送进程


-module(game_tcp_sender).

-export([start_link/2,send/2]).


%% ====================================================================
%% API functions
%% ====================================================================

%%发送消息
send(PlayerId,Bin)->
	Pid=realName(PlayerId),
	
	case is_pid(Pid) of
		true -> Pid ! {send,Bin};
		false -> {error,"game_tcp_sender is not started"}
	end.

%% ====================================================================
%% 启动
%% ====================================================================

%%启动发送soket进程
start_link(PlayerId,Socket) ->
	   {ok,proc_lib:spawn_link(?MODULE,init,[PlayerId,Socket])}.

%%初始化函数
init([PlayerId,Socket])->
	process_flag(trap_exit, true),
	register(realName(PlayerId),self()),
	wait_send(Socket).



%% ====================================================================
%% Internal functions
%% ====================================================================
realName(PlayerId) ->
	list_to_atom(PlayerId++"_tcp_sender").

%%发送消息
wait_send(Socket) -> 
	receive 
		{send,Data} -> gen_tcp:send(Socket,Data),
					   wait_send(Socket);
		{closed,Reason} -> exit(Reason)
	end.
