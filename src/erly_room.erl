%%%-------------------------------------------------------------------
%%% @author henry
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Feb 2018 下午2:54
%%%-------------------------------------------------------------------
-module(erly_room).
-author("henry").

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

%% API
-export([start_link/0]).



-record(state, {}).

%%===================================================
%% API
%%===================================================
%% @doc start the server
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, []).


%%===================================================
%% gen_server callbacks
%%===================================================

init([]) ->
  process_flag(trap_exit, true),
  {ok, #state{}}.

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
