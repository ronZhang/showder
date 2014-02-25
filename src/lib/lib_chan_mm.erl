%%负责消息接结构的组装
-module(lib_chan_cs).
-export([loop/2,send/2,close/1,controller/2,set_trace/2,trace_with_tag/2]).