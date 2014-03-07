%% @author Ron
%% @doc 游戏常量定义
-define(ETS_ONLINE,ets_online).
%%tcp配置
-define(TCP_OPTIONS, 
		[binary,
		  {packet, 0}, 
		  {active, false}, 
		  {reuseaddr, true},
		  {nodelay, false},
		  {delay_send, true},
		  {send_timeout, 5000},
		  {keepalive, true}, 
		  {exit_on_close, true}]).
-define(FL_POLICY_REQ, <<"<pol">>).
-define(FL_POLICY_FILE,<<"<cross-domain-policy><allow-access-from domain='*' to-ports='*' /></cross-domain-policy>">>).

