%% @author ron
%% @doc @todo Add description to mod_login.


-module(mod_login).
-include("record.hrl").
-include("common.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([login/3,login_out/1]).





%%TODO 登录
login(Parent,Client,Msg)->
	{Id,Password}=Msg,
    case Id==Password of 
		true -> Parent ! {login_success};
		false ->Parent ! {fail,login}
	end.


%%退出
login_out(Client)->
	PlayerPid=Client#client.player#player.id,
    PlayerPid ! {login_out,Client}.
	
	
