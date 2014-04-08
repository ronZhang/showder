%% @author Ron
%% @doc 游戏常量定义
%% ====================================================================
%% API functions
%% ====================================================================


%%服务器配置结构体
-record(serverConfig, {
        id,
        ip,
        port,
        node,
        num = 0
    }
).


%%玩家
-record(player,{id,name}).
-record(client,{
            player = none,
            login  = 0,
            accid  = 0,
            accname = none,
            timeout = 0 % 超时次数
     }).
