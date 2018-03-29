%%%-------------------------------------------------------------------
%%% @author henry
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Mar 2018 下午2:13
%%%-------------------------------------------------------------------
-module(erly_room_game_sup).
-author("henry").

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(GameID) ->
	supervisor:start_child(?MODULE, [GameID]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
	RoomSup = {erly_room_sup,
		{erly_room_sup, start_link, []},
		permanent, infinity, supervisor, [erly_room_sup]},

	{ok, {{simple_one_for_one, 10, 20}, [RoomSup]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
