%%%-------------------------------------------------------------------
%%% @author henry
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Mar 2018 下午5:55
%%%-------------------------------------------------------------------
-module(erly_room_handle_test).
-author("henry").

-include_lib("eunit/include/eunit.hrl").

-define(GAMEID, 100).

erly_room_handle_test_() ->
	{setup,
		fun start/0,
		fun stop/1,
		fun (SetupData) ->
			[
				user_join(SetupData)
			]
		end
	}.

start() ->
	erly_room_app:start_tree(?GAMEID),
	?GAMEID.

user_join(GameID) ->
	erly_room_app:user_join(GameID, "001").
