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
  code_change/3]).

%% API
-export([
  start_link/0,
  user_in/1,
  user_out/2
]).

-include_lib("erly_room.hrl").

-record(state, {}).

%%===================================================
%% API
%%===================================================
%% @doc start the server
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc 用户进入房间
user_in(UserID) ->
  gen_server:call(?MODULE, {user_in, UserID}).

%% @doc 用户退出房间
user_out(RoomID, UserID) ->
  gen_server:call(?MODULE, {user_out, RoomID, UserID}).

%%===================================================
%% gen_server callbacks
%%===================================================

init([]) ->
  process_flag(trap_exit, true),
  {ok, #state{}}.

handle_call({user_in, UserID}, _From, State) ->
  join_room(UserID),
  {reply, ok, State};
handle_call({user_out, RoomID, UserID}, _From, State) ->
  out_room(RoomID, UserID),
  {reply, ok, State};
handle_call(Message, _From, State) ->
  {stop, {unknown_call, Message}, State}.

handle_cast(Message, State) ->
  {stop, {unknown_cast, Message}, State}.

handle_info(_Message, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%===================================================
%%% Internal functions
%%===================================================

join_room(UserID) ->
  MatchRoom = ets:foldl(
    fun({Room, List, _Pid}, Acc) ->
        ?IF(length(List) > ?FILTER_ROOM_MIN andalso length(List) < ?FILTER_ROOM_MAX, [Room|Acc], Acc)
    end,
    [],
    erly_room),
  RoomID =
    case MatchRoom of
      [] ->
        Func = ets:fun2ms(fun({A, B, _C}) when length(B) < ?ROOM_MAX -> A end),
        RoomList = ets:select(erly_room, Func),
        case RoomList of
          [] ->
            create_room();
          _ ->
            lists:min(RoomList)
        end;
      _ ->
        lists:min(MatchRoom)
    end,
  UserList = lists:flatten( ets:lookup_element(erly_room, RoomID, 2) ),
  ets:update_element(erly_room, RoomID, {2, [UserID|UserList]}).

create_room() ->
  {ok, Pid} = erly_room:start_link(),
  RoomIDList =
    ets:foldl(
      fun({Room, _List, _Pid}, Acc) ->
        [Room|Acc]
      end,
      [],
      erly_room),
  RoomID =
    case RoomIDList of
      [] ->
        ?INIT_ROOM_ID;
      _ ->
        lists:max(RoomIDList) + 1
    end,
  ets:insert(erly, {RoomID, [], Pid}),
  RoomID.

out_room(RoomID, UserID) ->
  UserList = ets:lookup_element(erly_room, RoomID, 2),
  NewUserList = lists:delete(UserID, UserList),
  ets:update_element(erly_room, RoomID, {2, NewUserList}).