%%%-------------------------------------------------------------------
%%% @author henry
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Feb 2018 下午1:35
%%%-------------------------------------------------------------------
-author("henry").
-include_lib("erly_base/include/logger.hrl").

-define(ROOM_IF(_Expr,_Then),case _Expr of true ->_Then;false -> false end).
-define(ROOM_IF(_Expr,_Then,_Else),case _Expr of true -> _Then;false -> _Else end).


-define(ROOM_MAX, 7).                                   %% 房间最大人数
-define(FILTER_ROOM_MIN, 0).                            %% 筛选优先进入房间最小人数
-define(FILTER_ROOM_MAX, 3).                            %% 筛选优先进入房间最大人数

-define(INIT_ROOM_ID, 10000).                           %% 初始房间ID

-define(ADD_ROBOT_TIME, util:rand(180, 240) * 1000).    %% 随机在180～240秒内插入机器人
-define(DEL_ROBOT_TIME, 120 * 1000).                    %% 在120秒踢出机器人

-define(ROBOT_EVENT, util:rand(60, 300)).        		%% 机器人每隔60～300秒触发一个随机事件


-define(JOIN, 0).                                       %% 加入房间
-define(LEAVE, 1).                                      %% 退出房间

-define(TRUE, 0).										%% 加入房间成功标识
-define(FALSE, 1).										%% 加入房间失败标识

-define(FLUSH_TIME, 1000).                              %% 事件刷新时间

-define(NONE_REWARD, 0).								%% 没有中奖
-define(BIG_REWARD, 1).									%% 大奖
-define(MYST_REWARD, 2).								%% 神秘奖池
-define(REWARD_GAME, 3).								%% 奖励游戏
-define(FREE_GAME, 4).									%% 免费游戏
-define(MID_REWARD, 5).									%% 中奖
-define(SMALL_REWARD, 6).								%% 小奖