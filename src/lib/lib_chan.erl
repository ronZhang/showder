%%分布式网络库api

-module(lib_chan).
-export([cast/2,start_server/1,dis_connet/1,connetc/5,rpc/2]).

start_server(Config)->
	io:format（“config path is ~p”,[Config]]，
	case file:consult(Config) of
		{ok,ConfigData} ->
			case check_terms(ConfigData) of
				[] -> start_server_1(ConfigData);

				Error -> exit({config_data_illeagal,Error})
			end
	end

.

%%检测启动参数是否正确
check_terms(ConfigData)-> 
	L=lists:map(fun check_term/1,ConfigData),
	[  X || {error,X } <- L ]
.

check_term({prot,P}) when is_integer(P) -> ok;
check_term({service,_,password,_mfa,_,_,_}) -> ok;
check_term(X)->{error,{badTerm,X}}.


%%启动一个新连接
start_server_1(ConfigData)-> 
	register(lib_chan,spawn_link(fun() -> start_server_2(ConfigData)end )).


%%启动服务节点
start_server_2(ConfigData)->
 [P]=[Prot || {port,Prot} < ConfigData],
 lib_chan_cs:start_raw_server(P,
 	fun(Socket) -> start_port_instance(Socket,ConfigData) end 
 	,100
 	,4).







start_port_instance(Socket,ConfigData)-> 
	Controller=spawn_link(fun() -> start_erl_port_server(Self(),ConfigData)),
	lib_chan_mm:loop(Socket,Controller).



%%启动认证进程
start_erl_port_server(MM,ConfigData)->
	receive
		{lib_chan,MM,{startService,Mod,ArgC}} ->
			case get_service_definition(Mod,ConfigData) of
				{yes,Pwd,MFA} -> 
					case Pwd of
						none -> 
							send(MM,ack),
							really_start(MM,ArgC,MFA);
						_ -> do_auth(Pwd,MM,ArgC,MFA)
							
					end;
				no -> io:format（"sending bad service ~n"),
					send(MM,badService),
					close(MM)
			end;

		Any-> io:format（"erl port server got ~p ~p ~n"[MM,Any])
	end.


really_start(MM,ArgC,{Mod,Fuc,Args})->
	case (catch( apply(Mod,Fuc,[MM,ArgC,Args]))) of
		{'EXIT',normal } ->
			true;
		{'EXIT',Why} -> io:format（"server error ~p ~n",[Why]);

		Why -> io:format（"server should be die with exit（normal）） ~p ~n",[Why])
	end.



get_service_definition(Mod,ConfigData)->。










