-module(bs_server_tests).

-include_lib("eunit/include/eunit.hrl").
-include("bs_data.hrl").

-compile(export_all).

-define(SETUP(F), {setup, fun start/0, fun stop/1, F}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% ===============    Tests descriptions    ============= %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new_session_test_() ->
    [
        {"New session should have initiated id", 
            ?SETUP(fun is_session_id_initiated/1)},
        {"New session should have current game states initiated",
            ?SETUP(fun are_game_states_initiated/1)},
        {"New session should have current game states initiated properly",
            ?SETUP(fun are_game_states_initiated_properly/1)},
        {"New session should have exactly one game state for each known game type",
            ?SETUP(fun has_game_states_of_each_type/1)},
        {"New session should have players initiated",
            ?SETUP(fun are_players_initated/1)},
        {"New players should have different ids",
            ?SETUP(fun do_players_have_different_ids/1)},
        {"New players should cover all 4 possible positions",
            ?SETUP(fun do_players_cover_all_positions/1)},
        {"New players should have names assigned",
            ?SETUP(fun do_players_have_names_assigned/1)},
        {"New session should have empty history initiated",
            ?SETUP(fun is_empty_history_initiated/1)}
    ].

get_session_test_() ->
    [
        {"Get session should return session with given atom id",
            ?SETUP(fun do_session_has_given_id/1)},
        {"Get session should throw exception if given not-atom id",
            ?SETUP(fun are_only_atom_ids_accepted/1)},
        {"Get session should return proper session",
            ?SETUP(fun is_returning_proper_session/1)}
    ].

new_game_test_() ->
    [
        {"New game should return session",
            ?SETUP(fun do_return_session/1)},
        {"New game should return session with properly initiated game state",
            ?SETUP(fun do_return_properly_initiated_game_state/1)},
        {"New game should have impact on only one game type",
            ?SETUP(fun impacts_only_given_game_type/1)},
        {"New game should return empty game state, initiated just as game states in new session",
            ?SETUP(fun do_create_empty_game_state_just_as_in_new_session/1)},
        {"New game should be put in current games of session",
            ?SETUP(fun do_puts_new_game_in_sessions_current_games/1)},
        {"invoking new game should move previous game to history",
            ?SETUP(fun do_moves_prev_game_to_history/1)}
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% =================    Setup functions    ============== %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    bs_server:start_link().

stop(_Arg) ->
    bs_server:stop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% =================      Actual tests     ============== %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% ============================================================================================================
%%  Tests for function new_session 
%% ============================================================================================================
is_session_id_initiated(_) ->
    Session = bs_server:new_session(),
    [?_assertNotEqual(undefined, Session#bridge_session.id) ].

are_game_states_initiated(_) ->
    Session = bs_server:new_session(),
    [?_assertNotEqual([], Session#bridge_session.games_states)].

are_game_states_initiated_properly(_) ->
    Session = bs_server:new_session(),
    [ is_properly_initiated(GS) || {_, GS} <- Session#bridge_session.games_states].

has_game_states_of_each_type(_) ->
    Session = bs_server:new_session(),
    Types = lists:map(fun({_, GS}) -> GS#game_state.game_type end, Session#bridge_session.games_states),
    [
        ?_assertEqual(length([rubber, sport, imp]), length(Types)),
        ?_assert(lists:member(rubber, Types)),
        ?_assert(lists:member(sport, Types)),
        ?_assert(lists:member(imp, Types))
    ].

are_players_initated(_) ->
    Session = bs_server:new_session(),
    Players = Session#bridge_session.players,
    [
        ?_assertNotEqual([], Players),
        ?_assertMatch([_P1,_P2,_P3,_P4], Players)   % there should be exactly 4 players
    ].

do_players_have_different_ids(_) ->
    Session = bs_server:new_session(),
    [P1, P2, P3, P4] = Session#bridge_session.players,
    [
        ?_assertNotEqual(undefined, P1#player.id),
        ?_assertNotEqual(P2#player.id, P1#player.id),
        ?_assertNotEqual(P3#player.id, P1#player.id),
        ?_assertNotEqual(P4#player.id, P1#player.id),
        ?_assertNotEqual(undefined, P2#player.id),
        ?_assertNotEqual(P3#player.id, P2#player.id),
        ?_assertNotEqual(P4#player.id, P2#player.id),
        ?_assertNotEqual(undefined, P3#player.id),
        ?_assertNotEqual(P4#player.id, P3#player.id),
        ?_assertNotEqual(undefined, P4#player.id)
    ].

do_players_cover_all_positions(_) ->
    Session = bs_server:new_session(),
    [P1, P2, P3, P4] = Session#bridge_session.players,
    % fixed positions because of lazyness (and test is much more clear)
    [
        ?_assertEqual(north, P1#player.position),
        ?_assertEqual(south, P2#player.position),
        ?_assertEqual(west, P3#player.position),
        ?_assertEqual(east, P4#player.position)
    ].

do_players_have_names_assigned(_) ->
    Session = bs_server:new_session(),
    [P1, P2, P3, P4] = Session#bridge_session.players,
    [
        ?_assertNotEqual(undefined, P1#player.name),
        ?_assertNotEqual(undefined, P2#player.name),
        ?_assertNotEqual(undefined, P3#player.name),
        ?_assertNotEqual(undefined, P4#player.name)
    ].

is_empty_history_initiated(_) ->
    Session = bs_server:new_session(),
    EmptyHistory = #history{},
    [?_assertEqual(EmptyHistory, Session#bridge_session.history)].
%% ============================================================================================================
%%  Tests for function get_session
%% ============================================================================================================
do_session_has_given_id(_) ->
    S1 = bs_server:get_session(abc),
    S2 = bs_server:get_session(test),
    [
        ?_assertEqual(abc, S1#bridge_session.id),
        ?_assertEqual(test, S2#bridge_session.id)
    ].

are_only_atom_ids_accepted(_) ->
    [
        ?_assertError(badarg, bs_server:get_session("string")),
        ?_assertError(badarg, bs_server:get_session(<<"binary">>)),
        ?_assertError(badarg, bs_server:get_session(1234)),
        ?_assertError(badarg, bs_server:get_session(self()))
    ].

is_returning_proper_session(_) ->
    S1 = bs_server:new_session(),
    S2 = bs_server:new_session(),
    S3 = bs_server:new_session(),
    S4 = bs_server:new_session(),
    S5 = bs_server:new_session(),
    S6 = bs_server:get_session(abcd),
    [
        ?_assertEqual(S1, bs_server:get_session(S1#bridge_session.id)),
        ?_assertEqual(S2, bs_server:get_session(S2#bridge_session.id)),
        ?_assertEqual(S3, bs_server:get_session(S3#bridge_session.id)),
        ?_assertEqual(S4, bs_server:get_session(S4#bridge_session.id)),
        ?_assertEqual(S5, bs_server:get_session(S5#bridge_session.id)),
        ?_assertEqual(S6, bs_server:get_session(abcd))
    ].


%% ============================================================================================================
%%  Tests for function new_game
%% ============================================================================================================
do_return_session(_) ->
    _S = bs_server:get_session(test),
    G1 = bs_server:new_game(test, rubber),
    G2 = bs_server:new_game(test, sport),
    G3 = bs_server:new_game(test, imp),
    [
        ?_assertMatch(#bridge_session{}, G1),
        ?_assertMatch(#bridge_session{}, G2),
        ?_assertMatch(#bridge_session{}, G3)
    ].

do_return_properly_initiated_game_state(_) ->
    _S = bs_server:get_session(test),
    S1 = bs_server:new_game(test, rubber),
    S2 = bs_server:new_game(test, sport),
    S3 = bs_server:new_game(test, imp),
    [
        [is_properly_initiated(GS) || {_, GS} <- S1#bridge_session.games_states],
        [is_properly_initiated(GS) || {_, GS} <- S2#bridge_session.games_states],
        [is_properly_initiated(GS) || {_, GS} <- S3#bridge_session.games_states]
    ].

is_properly_initiated(#game_state{game_id=Id, game_type=Type, round_no=Round, score=Score, is_WE_vulnerable=WeV, is_NS_vulnerable=NsV, status=Status}) ->
    [
        ?_assertNotEqual(undefined, Id),
        ?_assert(lists:member(Type, [rubber, sport, imp])),
        ?_assertEqual(1, Round),
        ?_assertNot(WeV),
        ?_assertNot(NsV),
        ?_assertEqual(unfinished, Status),
        is_properly_initiated(Score)
    ];
is_properly_initiated(#score{above=Above, below=Below}) ->
    [
        ?_assertEqual([[]], Above),
        ?_assertEqual([], Below)
    ];
is_properly_initiated(#score_entry{count=Count, 'WE'=We, 'NS'=Ns}) ->
    [
        ?_assertEqual(0, Count),
        ?_assertEqual([], We),
        ?_assertEqual([], Ns)
    ].

impacts_only_given_game_type(_) ->
    [
    ].

do_create_empty_game_state_just_as_in_new_session(_) ->
    [
    ].

do_puts_new_game_in_sessions_current_games(_) ->
    [
    ].

do_moves_prev_game_to_history(_) ->
    [
    ].


%% ============================================================================================================
%%  Tests for function process_deal
%% ============================================================================================================


%% ============================================================================================================
%%  Tests for function remove_game
%% ============================================================================================================


%% ============================================================================================================
%%  Tests for function remove_deal
%% ============================================================================================================


%% ============================================================================================================
%%  Tests for function set_player_name
%% ============================================================================================================


%% ============================================================================================================
%%  Tests for function switch_players
%% ============================================================================================================

