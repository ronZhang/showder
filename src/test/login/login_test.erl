%% @author ron
%% @doc @todo Add description to login_test.


-module(login_test).
%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0]).



%% ====================================================================
%% Internal functions
%% ====================================================================

start()-> 
	case gen_tcp:connect("127.0.0.1",8080,[]) of
		{ok, Socket} -> sendData(Socket);
		{error, Reason} -> {error,Reason}
	end.

sendData(Socket)-> 
%% 	Data=simple_pb:encode_person({person,<<"Nick">>, <<"Mountain View">>,
%%     <<"+1 (000) 555-1234">>,25}),
	gen_tcp:send(Socket,pack(10000,term_to_binary({amao,amao}) )),
	gen_tcp:close(Socket).

pack(Cmd, Data) ->
    L = byte_size(Data) + 4,
    <<L:16, Cmd:16, Data/binary>>.




	
