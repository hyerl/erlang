%%%-------------------------------------------------------------------
%%% @author henry
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Feb 2018 下午2:29
%%%-------------------------------------------------------------------
-module(room_sup).
-author("henry").

%% API
-export([
  start_link/0,
  init/1
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Room = {erly_room,
          {erly_room, start_link, []},
          temporary, brutal_kill, worker, [erly_room]},

  Processes = [
              Room
              ],

  {ok, {{simple_one_for_one, 0, 1}, Processes}}.