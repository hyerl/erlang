%%%-------------------------------------------------------------------
%% @doc erly_room top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erly_room_sup).
-author("henry").

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

start_link(GameID) ->
	Name = list_to_atom(lists:concat([?MODULE, "_", GameID])),
	supervisor:start_link({local, Name}, ?MODULE, [GameID]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([GameID]) ->

	RoomManage = {erly_room_manage,
		{erly_room_manage, start_link, [GameID]},
		permanent, 5000, worker, [erly_room_manage]},

	RoomSup = {erly_room_worker_sup,
		{erly_room_worker_sup, start_link, [GameID]},
		permanent, infinity, supervisor, [erly_room_worker_sup]},

	Processes = [
		RoomManage,
		RoomSup
%%                Robot
	],

	{ok, { {one_for_all, 10, 5}, Processes} }.
