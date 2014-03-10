%%%------------------------------------------------
%%% File    : common.hrl
%%% Author  : xyao
%%% Created : 2010-04-22
%%% Description: 公共定义
%%%------------------------------------------------
-define(ALL_SERVER_PLAYERS, 10000).

%%安全校验
-define(TICKET, "SDFSDESF123DFSDF").

%%flash843安全沙箱
-define(FL_POLICY_REQ, <<"<pol">>).
%-define(FL_POLICY_REQ, <<"<policy-file-request/>\0">>).
-define(FL_POLICY_FILE, <<"<cross-domain-policy><allow-access-from domain='*' to-ports='*' /></cross-domain-policy>">>).

%%tcp_server监听参数
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}, {nodelay, false}, {delay_send, true}, {send_timeout, 5000}, {keepalive, true}, {exit_on_close, true}]).

%%错误处理
-define(DEBUG(F, A), util:log("debug", F, A, ?MODULE, ?LINE)).
-define(INFO(F, A), util:log("info", F, A, ?MODULE, ?LINE)).
-define(ERR(F, A), util:log("error", F, A, ?MODULE, ?LINE)).


%%数据库连接
-define(DB, sd_mysql_conn).
-define(DB_HOST, "localhost").
-define(DB_PORT, 3306).
-define(DB_USER, "sdzmmo").
-define(DB_PASS, "sdzmmo123456").
-define(DB_NAME, "sdzmmo").
-define(DB_ENCODE, utf8).

-define(DIFF_SECONDS_1970_1900, 2208988800).
-define(DIFF_SECONDS_0000_1900, 62167219200).
-define(ONE_DAY_SECONDS,        86400).

