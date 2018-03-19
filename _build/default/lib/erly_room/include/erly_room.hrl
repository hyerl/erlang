%%%-------------------------------------------------------------------
%%% @author henry
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Feb 2018 下午5:30
%%%-------------------------------------------------------------------
-author("henry").

-define(IF(_Expr,_Then),case _Expr of true ->_Then;false -> false end).
-define(IF(_Expr,_Then,_Else),case _Expr of true -> _Then;false -> _Else end).

-define(ROOM_MAX, 7).               %% 房间最大人数
-define(FILTER_ROOM_MIN, 7).        %% 筛选优先进入房间最小人数
-define(FILTER_ROOM_MAX, 7).        %% 筛选优先进入房间最大人数

-define(INIT_ROOM_ID, 10000).       %% 初始房间ID