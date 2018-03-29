%%%-------------------------------------------------------------------
%%% @author henry
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Mar 2018 下午5:34
%%%-------------------------------------------------------------------
-module(erly_room).
-author("henry").

%% API
-export([
	start_tree/1,
	user_join/3,
	user_leave/3,
	change_room/3,
	get_manage_state/1,
	get_room_state/2,
	push_info/3
]).

%% ========================================================================================
%%                                      外部 API
%% ========================================================================================

%% @doc 启动游戏进程树
-spec start_tree(GameID :: integer()) -> pid().
start_tree(GameID) ->
	erly_room_game_sup:start_child(GameID).

%% @doc 用户加入房间
-spec user_join(GameID :: integer(), UserInfo :: map(), RoomID :: undefined | integer()) -> RoomID :: integer().
user_join(GameID, UserInfo, RoomID) ->
	erly_room_manage:user_join(GameID, UserInfo, RoomID).

%% @doc 用户退出房间
-spec user_leave(GameID :: integer(), RoomID :: integer(), UserID :: string()) -> ok.
user_leave(GameID, RoomID, UserID) ->
	erly_room_manage:user_leave(GameID, RoomID, UserID).

%% @doc 用户更换房间
-spec change_room(GameID :: integer(), RoomID :: integer(), UserID :: string()) -> RoomID :: integer().
change_room(GameID, RoomID, #{user_id := UserID} = UserInfo) ->
	user_leave(GameID, RoomID, UserID),
	user_join2(GameID, UserInfo, RoomID).

%% @doc 查看erly_room_manage中state的状态
-spec get_manage_state(GameID :: integer()) -> State :: term().
get_manage_state(GameID) ->
	erly_room_manage:get_state(GameID).

%% @doc 查看erly_room_worker中state的状态
-spec get_room_state(GameID :: integer(), RoomID :: integer()) -> State :: term().
get_room_state(GameID, RoomID) ->
	erly_room_worker:get_state(erly_room_worker:get_pid(GameID, RoomID)).

%% @doc 推送玩家中奖信息
-spec push_info(UserInfo :: map(), GameID :: integer(), RoomID :: integer()) -> ok.
push_info(UserInfo, GameID, RoomID) ->
	erly_room_worker:push(UserInfo, GameID, RoomID).

%% ========================================================================================
%%                                      内部 API
%% ========================================================================================
-spec user_join2(GameID :: integer(), UserInfo :: map(), RoomID :: undefined | integer()) -> RoomID :: integer().
user_join2(GameID, UserInfo, RoomID) ->
	erly_room_manage:user_join2(GameID, UserInfo, RoomID).
