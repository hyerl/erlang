%%%-------------------------------------------------------------------
%% @doc erly_room top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erly_room_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->

    ets:new(erly_room, [set, public, named_table]),

    RoomManage = {erly_room_manage,
                    {erly_room_manage, start_link, []},
                    permanent, 5000, worker, [erly_room_manage]},

    RoomSup = {room_sup,
                {room_sup, start_link, []},
                permanent, 5000, supervisor, dynamic},

    Processes = [
                RoomManage,
                RoomSup
                ],

    {ok, { {one_for_all, 10, 5}, Processes} }.

%%====================================================================
%% Internal functions
%%====================================================================
