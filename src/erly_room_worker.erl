%%%-------------------------------------------------------------------
%%% @author henry
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Feb 2018 下午2:54
%%%-------------------------------------------------------------------
-module(erly_room_worker).
-author("henry").

-behaviour(gen_server).

%% gen_server callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

%% API
-export([
	start_link/1,
	push/3,
	add_robot/1,
	get_name/2,
	get_pid/2,
%%	user_join/3,
	async_msg/3
]).

-export([get_state/1]).

%% internal functions
-export([push_reward_to_single_user/4]).

-include_lib("erly_room.hrl").

-record(room_state, {
	game_id,
	room_id,
	user_list = [],
	robot_list = []
}).

-record(user_list, {
	user_id,
	user_name,
	icon
}).

-record(robot_list, {
	robot_id,
	robot_name,
	icon,
	time_out,
	event
}).

%%===================================================
%% API
%%===================================================
%% @doc start the server
start_link({GameID, RoomID}) ->
	gen_server:start_link(?MODULE, [GameID, RoomID],[]).

%% @doc 推送玩家中奖信息
push(UserInfo, GameID, RoomID) ->
	Pid = get_pid(GameID, RoomID),
	gen_server:cast(Pid, {push, GameID, UserInfo}).

add_robot(Pid) ->
	gen_server:call(Pid, add_robot).

get_state(Pid) ->
	gen_server:call(Pid, get_state).

async_msg(GameID, RoomID, Msg) ->
	gen_server:cast(get_pid(GameID, RoomID), Msg).

get_name(GameID, RoomID) ->
	{?MODULE, GameID, RoomID}.

get_pid(GameID, RoomID) ->
	gproc:lookup_local_name(get_name(GameID, RoomID)).

%%===================================================
%% gen_server callbacks
%%===================================================

init([GameID, RoomID]) ->
	gproc:add_local_name(get_name(GameID, RoomID)),
	State = #room_state{game_id = GameID, room_id = RoomID},
	erlang:send_after(?FLUSH_TIME, self(), flush),
	{ok, State}.

handle_call(add_robot, _From, State) ->
	erlang:send_after(?ADD_ROBOT_TIME, self(), add_robot),
	{reply, ok, State};

handle_call(get_state, _From, State) ->
	{reply, State, State};

handle_call(Message, _From, State) ->
	?ERROR("get msg ~p", [Message]),
	{stop, {unknown_call, Message}, State}.

handle_cast({user_join, RoomID, #{user_id := UserID} = UserInfo}, #room_state{game_id = GameID, user_list = UserList0, robot_list = RobotList} = State) ->
	UserList = filter_user_list_get_user_id(UserList0),

	NewState =
		case lists:member(UserID, UserList) of
			false ->
				is_room_full(UserInfo, RoomID, State);
			_ ->
				Len = length(UserList),
				erly_room_manage:refresh_room_num(GameID, {refresh_room_num, user_join, ?FALSE, RoomID, UserInfo, Len}),
				UserList1 = return_user_info(UserList0),
				UserList2 = proplists:delete(UserID, UserList1),
				RobotList1 = return_robot_info(RobotList),
				NewList = UserList2 ++ RobotList1,
				SendData = #{
					cmd => joinRoomResp,
					msg => #{
						player_list => [#{user_id => ID, user_name => mask_name(Name), icon_id => I} || {ID, Name, I} <- NewList]
					}
				},
				catch erly_player_send:send_player(UserID, GameID, SendData),
				State
		end,
	?INFO("erly_room ~p State ~p", [RoomID, NewState]),
	{noreply, NewState};

handle_cast({user_leave, RoomID, UserID}, #room_state{game_id = GameID, user_list = UserList0, robot_list = RobotList} = State) ->
	UserList = filter_user_list_get_user_id(UserList0),

	NewState =
		case lists:member(UserID, UserList) of
			true ->
				UserInfo = lists:keyfind(UserID, #user_list.user_id, UserList0),
				NewUserList = UserList0 -- [UserInfo],
				Len = length(NewUserList),
				RobotListInfo = lists:map(fun(Robot) -> {Robot#robot_list.robot_id, Robot#robot_list.robot_name, Robot#robot_list.icon} end, RobotList),
				#user_list{user_id = UserIDL, user_name = UserNameL, icon = IconL} = UserInfo,
				UserInfo0 = {UserIDL, UserNameL, IconL},
				broadcast(?LEAVE, GameID, NewUserList, RobotListInfo, UserInfo0),
				Info0 = #{user_id => UserIDL, user_name => UserNameL},
				erly_room_manage:refresh_room_num(GameID, {refresh_room_num, user_leave, ?TRUE, RoomID, Info0, Len}),
				case Len of
					0 ->
						State#room_state{user_list = NewUserList, robot_list = []};
					_ ->
						erlang:send_after(?ADD_ROBOT_TIME, self(), add_robot),
						State#room_state{user_list = NewUserList}
				end;
			_ ->
				State
		end,
	{noreply, NewState};

handle_cast({push, GameID, UserInfo}, #room_state{user_list = UserList0} = State) ->
	#{user_id := UserID, show_type := SlotResult} = UserInfo,
	UserList = filter_user_list_get_user_id(UserList0),
	NewUserList = UserList -- [UserID],
	lists:foreach(
		fun(X) ->
			push_reward_to_single_user(X, UserID, GameID, SlotResult)
		end,
		NewUserList),
	{noreply, State};

handle_cast(Message, State) ->
	?ERROR("get msg ~p", [Message]),
	{stop, {unknown_cast, Message}, State}.

handle_info(add_robot, #room_state{game_id = GameID, user_list = UserList, robot_list = RobotList} = State) ->
	UserNum = length(UserList),
	RobotNum = length(RobotList),
	NewRobotList = handle_robot(GameID, UserList, UserNum, RobotNum, RobotList),
	NewState = State#room_state{robot_list = NewRobotList},
	{noreply, NewState};

handle_info(del_robot, #room_state{game_id = GameID, user_list = UserList, robot_list = RobotList} = State) ->
	Robot =
		case length(RobotList) of
			0 ->
				[];
			_ ->
				lists:min(RobotList)
		end,
	#robot_list{robot_id = RobotID} =
		case Robot of
			[] ->
				#robot_list{robot_id = []};
			_ ->
				Robot
		end,
	NewRobotList =
		case RobotID of
			[] ->
				[];
			_ ->
				RobotIDList = lists:map(fun(R) -> R#robot_list.robot_id end, RobotList),
				RobotInfo = lists:keyfind(RobotID, #robot_list.robot_id, RobotList),
				#robot_list{robot_id = RID, robot_name = RN, icon = I} = RobotInfo,
				NewInfo = {RID, RN, I},
				broadcast(?LEAVE, GameID, UserList, RobotIDList, NewInfo),
				lists:delete(Robot, RobotList)
		end,
	NewState = State#room_state{robot_list = NewRobotList},
	erlang:send_after(?DEL_ROBOT_TIME, self(), del_robot),
	{noreply, NewState};

handle_info(flush, #room_state{game_id = GameID, room_id = RoomID, robot_list = RobotList} = State) ->
	Time = erlang:system_time(1),
	NewState =
		case RobotList of
			[] ->
				State;
			_ ->
				WinRobotList =
					lists:foldl(
						fun(#robot_list{robot_id = RobotID, robot_name = RobotName, icon = Icon, time_out = TimeOut, event = Event}, Acc) ->
							?ROOM_IF(TimeOut =< Time, [{RobotID, RobotName, Icon, Event}|Acc], Acc)
						end,
						[],
						RobotList),
				case WinRobotList of
					[] ->
						State;
					_ ->
						NewRobotList =
							lists:map(
								fun({RobotID, RobotName, Icon, Event}) ->
									UserInfo = #{user_id => RobotID, show_type => Event},
									push(UserInfo, GameID, RoomID),
									NewTimeOut = erlang:system_time(1) + ?ROBOT_EVENT,
									NewEvent = util:random_list([1,2,4,5,6]),		%% todo
									#robot_list{robot_id = RobotID, robot_name = RobotName, icon = Icon, time_out = NewTimeOut, event = NewEvent}
								end,
								WinRobotList),
						NewRobotList1 =
							lists:foldl(
								fun(#robot_list{robot_id = RobotID} = X, Acc) ->
									lists:keyreplace(RobotID, #robot_list.robot_id, Acc, X)
								end,
								RobotList,
								NewRobotList),
						State#room_state{robot_list = NewRobotList1}
				end
		end,
	erlang:send_after(?FLUSH_TIME, self(), flush),
	{noreply, NewState};

handle_info(Message, State) ->
	?ERROR("get msg ~p", [Message]),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%%===================================================
%% Internal functions
%%===================================================

push_reward_to_single_user(UserID, WinUserID, GameID, SlotResult) ->
	SendData = #{
		cmd => resultPush,
		msg => #{
			user_id => WinUserID,
			show_type => SlotResult
		}
	},
	catch erly_player_send:send_player(UserID, GameID, SendData),
	?INFO("push User ~p luck user ~p get slotresult ~p", [UserID, WinUserID, SlotResult]),
	ok.

broadcast(?JOIN, GameID, UserList0, RobotList, {UserID, UserName, Icon}) ->
	UserList = filter_user_list_get_user_id(UserList0),
	NewUserList = UserList -- [UserID],
	UserList1 = return_user_info(UserList0),
	UserList2 = proplists:delete(UserID, UserList1),
	RobotList1 = return_robot_info(RobotList),
	NewList = UserList2 ++ RobotList1,
	SendData = #{
		cmd => joinRoomResp,
		msg => #{
			player_list => [#{user_id => ID, user_name => mask_name(Name), icon_id => I} || {ID, Name, I} <- NewList]
		}
	},
	catch erly_player_send:send_player(UserID, GameID, SendData),
	SendData1 = #{
		cmd => syncJoinPush,
		msg => #{
			player => #{user_id => UserID, user_name => mask_name(UserName), icon_id => Icon}
		}
	},
	lists:foreach(
		fun(User) ->
			catch erly_player_send:send_player(User, GameID, SendData1)
		end,
		NewUserList);

broadcast(?LEAVE, GameID, UserList0, _RobotList, {UserID, UserName, Icon}) ->
	UserList = filter_user_list_get_user_id(UserList0),
	NewUserList = UserList -- [UserID],
	SendData = #{
		cmd => syncLeavePush,
		msg => #{
			player => #{user_id => UserID, user_name => mask_name(UserName), icon_id => Icon}
		}
	},
	lists:foreach(
		fun(User) ->
			catch erly_player_send:send_player(User, GameID, SendData)
		end,
		NewUserList);

broadcast(_Type, _GameID, _UserList, _RobotList, _UserID) ->
	ok.

handle_robot(GameID, UserList, UserNum, RobotNum, RobotList) when UserNum > 0 andalso UserNum < 3 andalso RobotNum < 3 ->
	RobotID = random_robot_id(),
	RobotName = random_robot_name(),
	UIconList = filter_user_list_get_user_icon(UserList),
	RIconList = filter_robot_list_get_robot_icon(RobotList),
	NewIcon = util:random_list( lists:seq(1, 12) -- UIconList ++ RIconList ),
	TimeOut = erlang:system_time(1) + ?ROBOT_EVENT,
	Event = util:random_list([1,2,4,5,6]),	%% todo
	?INFO("RobotID ~p join TimeOut ~p, Event ~p", [RobotID, TimeOut, Event]),
	NewRobotList = RobotList ++ [#robot_list{robot_id = RobotID, robot_name = RobotName, icon = NewIcon, time_out = TimeOut, event = Event}],
	RobotInfo = {RobotID, RobotName, NewIcon},
	broadcast(?JOIN, GameID, UserList, NewRobotList, RobotInfo),
	erlang:send_after(?ADD_ROBOT_TIME, self(), add_robot),
	NewRobotList;

handle_robot(_GameID, _UserList, UserNum, RobotNum, RobotList) when UserNum > 3 andalso RobotNum =/= 0 ->
	erlang:send_after(?DEL_ROBOT_TIME, self(), del_robot),
	RobotList;

handle_robot(_GameID, _UserList, UserNum, RobotNum, RobotList) when UserNum =< 3 orelse RobotNum =:= 0 orelse RobotNum =:= 3 ->
	RobotList;

handle_robot(_GameID, _UserList, _UserNum, _RobotNum, RobotList) ->
	erlang:send_after(?DEL_ROBOT_TIME, self(), del_robot),
	RobotList.

is_room_full(#{user_id := UserID, user_name := UserName} = UserInfo, RoomID, #room_state{game_id = GameID, user_list = UserList, robot_list = RobotList} = State) ->
	RoomSum = length(UserList) + length(RobotList),
	if
		RoomSum < ?ROOM_MAX ->
			UIconList = filter_user_list_get_user_icon(UserList),
			RIconList = filter_robot_list_get_robot_icon(RobotList),
			NewIcon = util:random_list( lists:seq(1, 12) -- UIconList ++ RIconList ),
			NewUserList = UserList ++ [#user_list{user_id = UserID, user_name = UserName, icon = NewIcon}],
			Len = length(NewUserList),
			NewUserInfo = {UserID, UserName, NewIcon},
			broadcast(?JOIN, GameID, NewUserList, RobotList, NewUserInfo),
			erly_room_manage:refresh_room_num(GameID, {refresh_room_num, user_join, ?TRUE, RoomID, UserInfo, Len}),
			erlang:send_after(?ADD_ROBOT_TIME, self(), add_robot),
			State#room_state{user_list = NewUserList};
		true ->
			State
	end.

filter_user_list_get_user_id([]) ->
	[];
filter_user_list_get_user_id(UserList) ->
	Fun = fun(#user_list{user_id = UserID}) -> UserID end,
	lists:map(Fun, UserList).

filter_user_list_get_user_icon([])->
	[];
filter_user_list_get_user_icon(UserList) ->
	Fun = fun(#user_list{icon = Icon}) -> Icon end,
	lists:map(Fun, UserList).

filter_robot_list_get_robot_icon([]) ->
	[];
filter_robot_list_get_robot_icon(RobotList) ->
	Fun = fun(#robot_list{icon = Icon}) -> Icon end,
	lists:map(Fun, RobotList).

random_robot_name() ->
	Str = "abcdefghijklmnopqrstuvwxyz0123456789",
	N = [rand:uniform(length(Str)) || _Elem <- lists:seq(1,17)],
	RandomKey = [lists:nth(X,Str) || X<- N ],
	Num = util:rand(7, 11),
	string:sub_string(RandomKey, Num).

random_robot_id() ->
	Str = "0123456789",
	N = [rand:uniform(length(Str)) || _Elem <- lists:seq(1,10)],
	[lists:nth(X,Str) || X<- N ].

return_user_info([]) ->
	[];
return_user_info(UserList) ->
	Fun = fun(#user_list{user_id = UserID, user_name = UserName, icon = Icon}) -> {UserID, UserName, Icon} end,
	lists:map(Fun, UserList).

return_robot_info([]) ->
	[];
return_robot_info(RobotList) ->
	Fun = fun(#robot_list{robot_id = RobotID, robot_name = RobotName, icon = Icon}) -> {RobotID, RobotName, Icon} end,
	lists:map(Fun, RobotList).

mask_name(undefined) ->
	[];
mask_name([]) ->
	[];
mask_name(Name) ->
	Str = string:sub_string(Name, 5),
	"****" ++ Str.