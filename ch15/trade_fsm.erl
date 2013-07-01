-module(trade_fsm).
-behavior(gen_fsm).
%% Public API
-export([start/1, start_link/1, trade/2, accept_trade/1,
         make_offer/2, retract_offer/2, ready/1, cancel/1]).
%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, cose_change/4,
         % states
         idle/2, idle/3, idle_wait/2, idle_wait/3, negotiate/2,
         negotiate/3, wait/2, ready/2, ready/3]).

%%% Public API
start(Name) -> gen_fsm:start(?MODULE, [Name], []).

start_link(Name) -> gen_fsm:start_link(?MODULE, [Name], []).

% request a session; returns when partner accepts
trade(OwnPid, OtherPid) -> 
  gen_fsm:sync_send_event(OwnPid, {negotiate, OtherPid}, 30000).

% accept a trade session request
accept_trade(OwnPid) -> 
  gen_fsm:sync_send_event(OwnPid, accept_negotiate).

% propose trading Item
make_offer(OwnPid, Item) ->
  gen_fsm:send_event(OwnPid, {make_offer, Item}).

% cancel trade offer
retract_offer(OwnPid, Item) ->
  gen_fsm:send_event(OwnPid, {retract_offer, Item}).

% signal readyness to trade
ready(OwnPid) -> 
  gen_fsm:sync_send_event(OwnPid, ready, infinity).

% cancel session
cancel(OwnPid) -> 
  gen_fsm:sync_send_all_state_event(OwnPid, cancel).


%%% FSM to FSM
% ask other fsm for a trade session
ask_negotiate(OtherPid, OwnPid) -> 
  gen_fsm:send_event(OtherPid, {ask_negotiate, OwnPid}).

% forward the client message accepting the transaction
accept_negotiate(OtherPid, OwnPid) ->
  gen_fsm:send_event(OtherPid, {accept_negotiate, OwnPid}).

% forward a client's offer
do_offer(OtherPid, Item) ->
  gen_fsm:send_event(OtherPid, {do_offer, Item}).

% forward a client's offer cancellation
undo_offer(OtherPid, Item) ->
  gen_fsm:send_event(OtherPid, {undo_offer, Item}).

% ask if ready to trade
are_you_ready(OtherPid) ->
  gen_fsm:send_event(OtherPid, are_you_ready).

% reply "not ready" (not in "wait" state)
not_yet(OtherPid) ->
  gen_fsm:send_event(OtherPid, not_yet).

% tell other fsm that user is waiting for ready state.
% state should transition to "ready"
am_ready(OtherPid) ->
  gen_fsm:send_event(OtherPid, 'ready!').

% ack ready state
ack_trans(OtherPid) ->
  gen_fsm:send_event(OtherPid, ack).

% ask if ready to commit
ask_commit(OtherPid) ->
  gen_fsm:sync_send_event(OtherPid, ask_commit).

% begin sync commit
do_commit(OtherPid) ->
  gen_fsm:sync_send_event(OtherPid, do_commit).

% notify ca=ncelled
notify_cancel(OtherPid) ->
  gen_fsm:send_all_state_event(OtherPid, cancel).





