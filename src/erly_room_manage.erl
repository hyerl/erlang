%%%-------------------------------------------------------------------
%%% @author henry
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Feb 2018 下午2:30
%%%-------------------------------------------------------------------
-module(erly_room_manage).
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
	user_join/3,
	user_join2/3,
	user_leave/3,
	get_name/1,
	get_pid/1,
	refresh_room_num/2
]).

-export([get_state/1]).

-include_lib("erly_room.hrl").

-record(state, {game_id, room_state = []}).

-record(room_state, {room_id, num}).

%% ========================================================================================
%%                                      外部 API
%% ========================================================================================

get_name(GameID) ->
	{?MODULE, GameID}.

get_pid(GameID) ->
	gproc:lookup_local_name(get_name(GameID)).

%% @doc start the server
start_link(GameID) ->
	Name = list_to_atom(lists:concat([?MODULE, "_", GameID])),
	gen_server:start_link({local, Name}, ?MODULE, [GameID], []).

%% @doc 用户进入房间
user_join(GameID, UserInfo, RoomID) ->
	gen_server:cast(get_pid(GameID), {user_join, join, UserInfo, RoomID}).

%% @doc 用户更换房间
user_join2(GameID, UserInfo, RoomID) ->
	gen_server:cast(get_pid(GameID), {user_join, return, UserInfo, RoomID}).

%% @doc 用户退出房间
user_leave(GameID, RoomID, UserID) ->
	Pid = get_pid(GameID),
	case is_pid(Pid) of
		true ->
			gen_server:cast(Pid, {user_leave, RoomID, UserID});
		_ ->
			ok
	end.

refresh_room_num(GameID, {refresh_room_num, Action, Type, RoomID, UserInfo, Len}) ->
	gen_server:cast(get_pid(GameID), {refresh_room_num, Action, Type, RoomID, UserInfo, Len}).

get_state(GameID) ->
	Pid = get_pid(GameID),
	gen_server:call(Pid, get_state).

%%===================================================
%% gen_server callbacks
%%===================================================

init([GameID]) ->
	gproc:add_local_name(get_name(GameID)),
	State = #state{game_id = GameID},
	{ok, State}.

handle_call(get_state, _From, State) ->
	{reply, State, State};

handle_call(Message, _From, State) ->
	{stop, {unknown_call, Message}, State}.

handle_cast({user_join, Type, UserInfo, RoomID0}, #state{game_id = GameID, room_state = RoomStateList} = State) ->
	{RoomID, NewRoomStateList} = select_room(Type, GameID, RoomStateList, RoomID0),
	NewState = State#state{room_state = NewRoomStateList},
	erly_room_worker:async_msg(GameID, RoomID, {user_join, RoomID, UserInfo}),
	{noreply, NewState};

handle_cast({user_leave, RoomID, UserID}, #state{game_id = GameID, room_state = RoomStateList} = State) ->
	Fun = fun(#room_state{room_id = Room, num = Num} = RoomState) ->
		Bool = string:equal(Room, RoomID),
		case Bool of
			true ->
				erly_room_worker:async_msg(GameID, RoomID, {user_leave, RoomID, UserID}),
				#room_state{room_id = Room, num = Num - 1};
			_ ->
				RoomState
		end
	end,
	NewRoomStateList = lists:map(Fun, RoomStateList),

	NewState = State#state{room_state = NewRoomStateList},
	?INFO("manage State ~p", [NewState]),
	{noreply, NewState};

handle_cast({refresh_room_num, user_join, Type, RoomID, #{user_id := UserID} = UserInfo, Len}, #state{game_id = GameID, room_state = RoomStateList} = State) ->
	NewRoomStateList = lists:keyreplace(RoomID, #room_state.room_id, RoomStateList, #room_state{room_id = RoomID, num = Len}),
	NewState = State#state{room_state = NewRoomStateList},
	?INFO("manage State ~p", [NewState]),
	case Type of
		?TRUE ->
			catch erly_player:cast(UserID, GameID, {room_join_ok, RoomID});
		?FALSE ->
			ok
	end,
	{noreply, NewState};

handle_cast({refresh_room_num, user_leave, _Type, RoomID, _UserID, Len}, #state{room_state = RoomStateList} = State) ->
	NewRoomStateList = lists:keyreplace(RoomID, #room_state.room_id, RoomStateList, #room_state{room_id = RoomID, num = Len}),
	NewState = State#state{room_state = NewRoomStateList},
	{noreply, NewState};

handle_cast(Message, State) ->
	{stop, {unknown_cast, Message}, State}.

handle_info(_Message, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%===================================================
%% Internal functions
%%===================================================

select_room(Type, GameID, RoomStateList0, RoomID0) ->
	Fun = fun(#room_state{room_id = RoomID, num = Num}, Acc) ->
		[{RoomID, Num}|Acc]
	end,
	RoomStateList = lists:foldl(Fun, [], RoomStateList0),

	{RoomID, NewRoomStateList} =
		case RoomStateList of
			[] ->
				create_room(GameID, RoomStateList);
			_ ->
				select_exist_room(Type, GameID, RoomStateList, RoomID0)
		end,
	{RoomID, NewRoomStateList}.

select_exist_room(join, GameID, RoomStateList0, RoomID0) ->
	RoomStateList =
		case RoomID0 of
			undefined ->
				RoomStateList0;
			_ ->
				[lists:keyfind(RoomID0, 1, RoomStateList0)]
		end,

	Fun = fun({RoomID1, Num}, Acc) ->
			?ROOM_IF(Num >= ?FILTER_ROOM_MIN andalso Num =< ?FILTER_ROOM_MAX, [{RoomID1, Num}|Acc], Acc)
	end,
	MatchRoomList = lists:foldl(Fun, [], RoomStateList),

	case MatchRoomList of
		[] ->
			Fun0 = fun({RoomID, Num}, Acc) ->
				?ROOM_IF(Num < ?ROOM_MAX, [{RoomID, Num}|Acc], Acc)
			end,
			MatchRoomList1 = lists:foldl(Fun0, [], RoomStateList),
			case MatchRoomList1 of
				[] ->
					create_room(GameID, RoomStateList0);
				_ ->
					{RoomID, Num} = lists:min(MatchRoomList1),
					Fun1 = fun({R, N}) ->
						#room_state{room_id = R, num = N}
						   end,
					RoomStateList1 = lists:map(Fun1, RoomStateList0),

					NewRoomStateList = lists:keyreplace(RoomID, 1, RoomStateList1, #room_state{room_id = RoomID, num = Num + 1}),
					{RoomID, NewRoomStateList}
			end;
		_ ->
			{RoomID, Num} = lists:min(MatchRoomList),
			Fun0 = fun({R, N}) ->
				#room_state{room_id = R, num = N}
			end,
			RoomStateList1 = lists:map(Fun0, RoomStateList0),

			NewRoomStateList = lists:keyreplace(RoomID, 1, RoomStateList1, #room_state{room_id = RoomID, num = Num + 1}),
			{RoomID, NewRoomStateList}
	end;

select_exist_room(return, GameID, RoomStateList0, RoomID0) ->
	RoomStateList =
		case RoomID0 of
			undefined ->
				RoomStateList0;
			_ ->
				lists:keydelete(RoomID0, 1, RoomStateList0)
		end,

	Fun = fun({RoomID1, Num}, Acc) ->
		?ROOM_IF(Num >= ?FILTER_ROOM_MIN andalso Num =< ?FILTER_ROOM_MAX, [{RoomID1, Num}|Acc], Acc)
		  end,
	MatchRoomList = lists:foldl(Fun, [], RoomStateList),

	case MatchRoomList of
		[] ->
			Fun0 = fun({RoomID, Num}, Acc) ->
				?ROOM_IF(Num < ?ROOM_MAX, [{RoomID, Num}|Acc], Acc)
				   end,
			MatchRoomList1 = lists:foldl(Fun0, [], RoomStateList),
			case MatchRoomList1 of
				[] ->
					create_room(GameID, RoomStateList0);
				_ ->
					{RoomID, Num} = lists:min(MatchRoomList1),
					Fun1 = fun({R, N}) ->
						#room_state{room_id = R, num = N}
						   end,
					RoomStateList1 = lists:map(Fun1, RoomStateList0),

					NewRoomStateList = lists:keyreplace(RoomID, 1, RoomStateList1, #room_state{room_id = RoomID, num = Num + 1}),
					{RoomID, NewRoomStateList}
			end;
		_ ->
			{RoomID, Num} = lists:min(MatchRoomList),
			Fun0 = fun({R, N}) ->
				#room_state{room_id = R, num = N}
				   end,
			RoomStateList1 = lists:map(Fun0, RoomStateList0),

			NewRoomStateList = lists:keyreplace(RoomID, 1, RoomStateList1, #room_state{room_id = RoomID, num = Num + 1}),
			{RoomID, NewRoomStateList}
	end.

create_room(GameID, RoomStateList) ->
	RoomID =
		case RoomStateList of
			[] ->
				?INIT_ROOM_ID;
			_ ->
				RoomInfo = lists:max(RoomStateList),
				{ID, _Num} = RoomInfo,
				ID + 1
		end,
	erly_room_worker_sup:start_child(GameID, {GameID, RoomID}),
	NewRoomStateList =
		case RoomStateList of
			[] ->
				[];
			_ ->
				Fun = fun({R, N}) ->
					#room_state{room_id = R, num = N}
				end,
				lists:map(Fun, RoomStateList)
		end,
	{RoomID, NewRoomStateList ++ [#room_state{room_id = RoomID, num = 0}]}.
