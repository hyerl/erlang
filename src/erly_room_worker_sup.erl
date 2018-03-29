%%%-------------------------------------------------------------------
%%% @author henry
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Feb 2018 下午2:29
%%%-------------------------------------------------------------------
-module(erly_room_worker_sup).
-author("henry").

%% API
-export([
	start_link/1,
	init/1,
	start_child/2,
	get_name/1,
	get_pid/1
]).

%%====================================================================
%% API functions
%%====================================================================

start_child(GameID, Args) ->
	Pid = get_pid(GameID),
	supervisor:start_child(Pid, [Args]).

start_link(GameID) ->
	Name = list_to_atom(lists:concat([?MODULE, "_", GameID])),
	supervisor:start_link({local, Name}, ?MODULE, [GameID]).

get_name(GameID) ->
	{?MODULE, GameID}.

get_pid(GameID) ->
	gproc:lookup_local_name(get_name(GameID)).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([GameID]) ->
	gproc:add_local_name(get_name(GameID)),
	Room = {erly_room_worker,
		{erly_room_worker, start_link, []},
		permanent, 2000, worker, [erly_room_worker]},

	Processes = [
		Room
	],

	{ok, {{simple_one_for_one, 10, 20}, Processes}}.

