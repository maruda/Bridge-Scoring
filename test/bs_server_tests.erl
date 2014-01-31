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
            ?SETUP(fun ns_is_session_id_initiated/1)},
        {"New session should have current game states initiated",
            ?SETUP(fun ns_are_game_states_initiated/1)},
        {"New session should have current game states initiated properly",
            ?SETUP(fun ns_are_game_states_initiated_properly/1)},
        {"New session should have exactly one game state for each known game type",
            ?SETUP(fun ns_has_game_states_of_each_type/1)},
        {"New session should have players initiated",
            ?SETUP(fun ns_are_players_initated/1)},
        {"New players should have different ids",
            ?SETUP(fun ns_do_players_have_different_ids/1)},
        {"New players should cover all 4 possible positions",
            ?SETUP(fun ns_do_players_cover_all_positions/1)},
        {"New players should have names assigned",
            ?SETUP(fun ns_do_players_have_names_assigned/1)},
        {"New session should have empty history initiated",
            ?SETUP(fun ns_is_empty_history_initiated/1)}
    ].

get_session_test_() ->
    [
        {"Get session should return session with given atom id",
            ?SETUP(fun gs_do_session_has_given_id/1)},
        {"Get session should throw exception if given non-atom id",
            ?SETUP(fun gs_are_only_atom_ids_accepted/1)},
        {"Get session should return proper session",
            ?SETUP(fun gs_is_returning_proper_session/1)}
    ].

new_game_test_() ->
    [
        {"New game should return session",
            ?SETUP(fun ng_do_return_session/1)},
        {"New game should return session with properly initiated game state",
            ?SETUP(fun ng_do_return_properly_initiated_game_state/1)},
        {"New game should have impact on only one game type",
            ?SETUP(fun ng_impacts_only_given_game_type/1)},
        {"New game should return empty game state, initiated just as game states in new session",
            ?SETUP(fun ng_do_create_empty_game_state_just_as_in_new_session/1)},
        {"New game should be put in current games of session",
            ?SETUP(fun ng_do_puts_new_game_in_sessions_current_games/1)},
        {"Invoking new game should move previous game to history",
            ?SETUP(fun ng_do_moves_prev_game_to_history/1)},
        {"New game should throw exception if given non-atom session id",
            ?SETUP(fun ng_are_only_atom_session_ids_accepted/1)},
        {"New game should throw exception if given non-atom game type",
            ?SETUP(fun ng_are_only_atom_game_types_accepted/1)},
        {"New game should accept only game types that are one of [rubber, sport, imp]",
            ?SETUP(fun ng_are_only_known_game_types_accepted/1)},
        {"New game should throw error on unknown session id",
            ?SETUP(fun ng_is_error_thrown_on_unknown_session_id/1)}
    ].
process_deal_test_() ->
    [
        {"Should affect only game of given type",
            ?SETUP(fun pd_impacts_only_given_game_type/1)},
        {"Should increment round counter",
            ?SETUP(fun pd_incremants_round_counter/1)},
        {"Should create new game if processed deal was final for game",
            ?SETUP(fun pd_creates_new_game_after_finishing_one/1)},
        {"Should move closed game to history if created new one",
            ?SETUP(fun pd_moves_closed_game_to_history/1)}
        % Should throw error on non atom session id
    % should throw error on unknown session id
    % should accept only game types that are one of [rubber, sport, imp]
        % Should throw error on non atom game id
    ].

remove_game_test_() ->
    [
        {"Should remove only game wih given id",
            ?SETUP(fun rg_removes_only_game_with_given_id/1)},
        {"Should remove game form current games and from history (depending on where game is)",
            ?SETUP(fun rg_removes_games_regardless_they_are_current_or_in_history/1)},
        {"Removed current game should be replaced with new game",
            ?SETUP(fun rg_creates_new_game_on_removing_current_one/1)},
        {"Removing game from history should remove also players mapped to this game",
            ?SETUP(fun rg_removes_games_from_history_with_players_mapping/1)}
        % Should throw error on non atom session id
        % Should throw error on non atom game id
    % should throw error on unknown session id
    % should accept only game types that are one of [rubber, sport, imp]
    ].

remove_deal_test_() ->
    [
        {"Only last deal may be removed",
            ?SETUP(fun rd_removes_last_deal/1)},
        {"Deal may be remover only from current game",
            ?SETUP(fun rd_removes_deal_only_from_current_game/1)},
        {"Should decrement round counter",
            ?SETUP(fun rd_decrements_round_counter/1)},
        {"Sould reset vulnerability if needed",
            ?SETUP(fun rd_keeps_vulnerability_consistent/1)}
        % Should throw error on non atom session id
    % should throw error on unknown session id
        % Should throw error on non atom game id
    % should accept only game types that are one of [rubber, sport, imp]
    ].

set_player_name_test_() ->
    [
        % Should throw error on non atom session id
    % should throw error on unknown session id
        % Should accept only valid positions
    % should accept olny positions that are one of [north, south, east, west]
        {"Should change players name",
            ?SETUP(fun spn_changes_player_name/1)},
        {"Should affect only player with given position",
            ?SETUP(fun spn_affects_only_player_with_given_position/1)},
        {"Should not change players order",
            ?SETUP(fun spn_do_not_change_players_order/1)}
    ].

switch_players_test_() ->
    [
        % Should throw error on non atom session id
    % should throw error on unknown session id
        % Should accept only valid positions
    % should accept olny positions that are one of [north, south, east, west]
        {"Should affect only positions of two players",
            ?SETUP(fun sp_affects_only_positions_of_two_players/1)},
        {"After invokation all positions should be covered",
            ?SETUP(fun sp_are_all_positions_covered_after_invocation/1)}
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
ns_is_session_id_initiated(_) ->
    Session = bs_server:new_session(),
    [?_assertNotEqual(undefined, Session#bridge_session.id) ].

ns_are_game_states_initiated(_) ->
    Session = bs_server:new_session(),
    [?_assertNotEqual([], Session#bridge_session.games_states)].

ns_are_game_states_initiated_properly(_) ->
    Session = bs_server:new_session(),
    [ is_properly_initiated(GS) || {_, GS} <- Session#bridge_session.games_states].

ns_has_game_states_of_each_type(_) ->
    Session = bs_server:new_session(),
    Types = lists:map(fun({_, GS}) -> GS#game_state.game_type end, Session#bridge_session.games_states),
    [
        ?_assertEqual(length([rubber, sport, imp]), length(Types)),
        ?_assert(lists:member(rubber, Types)),
        ?_assert(lists:member(sport, Types)),
        ?_assert(lists:member(imp, Types))
    ].

ns_are_players_initated(_) ->
    Session = bs_server:new_session(),
    Players = Session#bridge_session.players,
    [
        ?_assertNotEqual([], Players),
        ?_assertMatch([_P1,_P2,_P3,_P4], Players)   % there should be exactly 4 players
    ].

ns_do_players_have_different_ids(_) ->
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

ns_do_players_cover_all_positions(_) ->
    Session = bs_server:new_session(),
    are_all_positions_covered(Session#bridge_session.players).

are_all_positions_covered(Players) ->
    CoveredPositions = [ P || #player{position=P} <- Players],
    [
        ?_assert(lists:member(north, CoveredPositions)),
        ?_assert(lists:member(south, CoveredPositions)),
        ?_assert(lists:member(west, CoveredPositions)),
        ?_assert(lists:member(east, CoveredPositions))
    ].

ns_do_players_have_names_assigned(_) ->
    Session = bs_server:new_session(),
    [P1, P2, P3, P4] = Session#bridge_session.players,
    [
        ?_assertNotEqual(undefined, P1#player.name),
        ?_assertNotEqual(undefined, P2#player.name),
        ?_assertNotEqual(undefined, P3#player.name),
        ?_assertNotEqual(undefined, P4#player.name)
    ].

ns_is_empty_history_initiated(_) ->
    Session = bs_server:new_session(),
    EmptyHistory = #history{},
    [?_assertEqual(EmptyHistory, Session#bridge_session.history)].

%% ============================================================================================================
%%  Tests for function get_session
%% ============================================================================================================
gs_do_session_has_given_id(_) ->
    S1 = bs_server:get_session(abc),
    S2 = bs_server:get_session(test),
    [
        ?_assertEqual(abc, S1#bridge_session.id),
        ?_assertEqual(test, S2#bridge_session.id)
    ].

gs_are_only_atom_ids_accepted(_) ->
    [
        ?_assertEqual({badarg, non_atom_id}, bs_server:get_session("string")),
        ?_assertEqual({badarg, non_atom_id}, bs_server:get_session(<<"binary">>)),
        ?_assertEqual({badarg, non_atom_id}, bs_server:get_session(1234)),
        ?_assertEqual({badarg, non_atom_id}, bs_server:get_session(self()))
    ].

gs_is_returning_proper_session(_) ->
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
ng_do_return_session(_) ->
    _S = bs_server:get_session(test),
    G1 = bs_server:new_game(test, rubber),
    G2 = bs_server:new_game(test, sport),
    G3 = bs_server:new_game(test, imp),
    [
        ?_assertMatch(#bridge_session{}, G1),
        ?_assertMatch(#bridge_session{}, G2),
        ?_assertMatch(#bridge_session{}, G3)
    ].

ng_do_return_properly_initiated_game_state(_) ->
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

ng_impacts_only_given_game_type(_) ->
    [
    ].

ng_do_create_empty_game_state_just_as_in_new_session(_) ->
    [
    ].

ng_do_puts_new_game_in_sessions_current_games(_) ->
    [
    ].

ng_do_moves_prev_game_to_history(_) ->
    [
    ].

ng_are_only_atom_session_ids_accepted(_) ->
    [
        ?_assertEqual({badarg, non_atom_id}, bs_server:new_game("string", rubber)),
        ?_assertEqual({badarg, non_atom_id}, bs_server:new_game(<<"binary_string">>, rubber)),
        ?_assertEqual({badarg, non_atom_id}, bs_server:new_game(1234, rubber)),
        ?_assertEqual({badarg, non_atom_id}, bs_server:new_game(self(), rubber)),
        ?_assertEqual({badarg, non_atom_id}, bs_server:new_game([list, 'of', atoms], rubber)),
        ?_assertEqual({badarg, non_atom_id}, bs_server:new_game({tuple, 'of', 4, "terms"}, rubber)),
        ?_assertNotEqual({badarg, non_atom_id}, bs_server:new_game(atom_id, rubber))
    ].

ng_are_only_atom_game_types_accepted(_) ->
    bs_server:get_session(id),
    [
        ?_assertEqual({badarg, non_atom_game_type}, bs_server:new_game(id, "string")),
        ?_assertEqual({badarg, non_atom_game_type}, bs_server:new_game(id, <<"binary string">>)),
        ?_assertEqual({badarg, non_atom_game_type}, bs_server:new_game(id, 1234)),
        ?_assertEqual({badarg, non_atom_game_type}, bs_server:new_game(id, self())),
        ?_assertEqual({badarg, non_atom_game_type}, bs_server:new_game(id, [list, "of", 4, terms])),
        ?_assertEqual({badarg, non_atom_game_type}, bs_server:new_game(id, {"tuple of", something})),
        % this invalid value should throw exception, but different
        ?_assertNotEqual({badarg, non_atom_game_type}, bs_server:new_game(id, atom)),
        % check valid values
        ?_assertNotEqual({badarg, non_atom_game_type}, bs_server:new_game(id, rubber)),
        ?_assertNotEqual({badarg, non_atom_game_type}, bs_server:new_game(id, sport)),
        ?_assertNotEqual({badarg, non_atom_game_type}, bs_server:new_game(id, imp))
    ].

ng_are_only_known_game_types_accepted(_) ->
    [
        ?_assertEqual({badarg, unknown_game_type}, bs_server:new_game(test, unknown)),
        ?_assertEqual({badarg, unknown_game_type}, bs_server:new_game(test, undefined)),
        ?_assertEqual({badarg, unknown_game_type}, bs_server:new_game(test, any_atom)),
        ?_assertNotEqual({badarg, unknown_game_type}, bs_server:new_game(test, rubber)),
        ?_assertNotEqual({badarg, unknown_game_type}, bs_server:new_game(test, sport)),
        ?_assertNotEqual({badarg, unknown_game_type}, bs_server:new_game(test, imp))
    ].

ng_is_error_thrown_on_unknown_session_id(_) ->
    [
        ?_assertEqual({badarg, unknown_session}, bs_server:new_game(some_id, rubber)),
        ?_assertEqual({badarg, unknown_session}, bs_server:new_game(some_id, sport)),
        ?_assertEqual({badarg, unknown_session}, bs_server:new_game(some_id, imp)),
        ?_assertEqual({badarg, unknown_session}, bs_server:new_game(other_id, imp))
    ].

%% ============================================================================================================
%%  Tests for function process_deal
%% ============================================================================================================
pd_impacts_only_given_game_type(_) ->
    [
    ].

pd_incremants_round_counter(_) ->
    [
    ].

pd_creates_new_game_after_finishing_one(_) ->
    [
    ].

pd_moves_closed_game_to_history(_) ->
    [
    ].

%% ============================================================================================================
%%  Tests for function remove_game
%% ============================================================================================================
rg_removes_only_game_with_given_id(_) ->
    [
    ].

rg_removes_games_regardless_they_are_current_or_in_history(_) ->
    [
    ].

rg_creates_new_game_on_removing_current_one(_) ->
    [
    ].

rg_removes_games_from_history_with_players_mapping(_) ->
    [
    ].

%% ============================================================================================================
%%  Tests for function remove_deal
%% ============================================================================================================
rd_removes_last_deal(_) ->
    [
    ].

rd_removes_deal_only_from_current_game(_) ->
    [
    ].

rd_decrements_round_counter(_) ->
    [
    ].

rd_keeps_vulnerability_consistent(_) ->
    [
    ].

%% ============================================================================================================
%%  Tests for function set_player_name
%% ============================================================================================================
spn_changes_player_name(_) ->
    S1 = bs_server:get_session(test),
    Players1 = S1#bridge_session.players,
    Players2 = bs_server:set_player_name(test, north, "ModifiedNorthName"), 
    Players3 = bs_server:set_player_name(test, south, "ModifiedSouthName"), 
    Players4 = bs_server:set_player_name(test, west, "ModifiedWestName"), 
    Players5 = bs_server:set_player_name(test, east, "ModifiedEastName"), 
    [
        has_name_been_changed(Players1, Players2, north),
        has_name_been_changed(Players2, Players3, south),
        has_name_been_changed(Players3, Players4, west),
        has_name_been_changed(Players4, Players5, east)
    ].

has_name_been_changed(LB, LA, Pos) ->
    GetModified = fun(L) -> lists:filter(fun(#player{position=P}) -> P==Pos end, L) end,
    [ModBefore] = GetModified(LB),
    [ModAfter] = GetModified(LA),
    ?_assertNotEqual(ModBefore#player.name, ModAfter#player.name).

spn_affects_only_player_with_given_position(_) ->
    S1 = bs_server:get_session(test),
    Players1 = S1#bridge_session.players,
    Players2 = bs_server:set_player_name(test, north, "NorthName"), 
    Players3 = bs_server:set_player_name(test, south, "SouthName"), 
    Players4 = bs_server:set_player_name(test, west, "WestName"), 
    Players5 = bs_server:set_player_name(test, east, "EastName"), 
    [
        was_only_given_player_modified(Players1, Players2, north),
        was_only_given_player_modified(Players2, Players3, south),
        was_only_given_player_modified(Players3, Players4, west),
        was_only_given_player_modified(Players4, Players5, east)
    ].

was_only_given_player_modified(Before, After, ModPos) ->
    % players order shouldnt change so zip merges same player
    Merged = lists:zip(Before, After),
    Test = fun({B,A}) ->
            case B#player.position of
                ModPos ->   % modified player can be skipped
                    ?_assert(true);
                _ ->    % unmodified player; A & B should be equal
                    ?_assertEqual(A,B)
            end
    end,        
    [ Test(X) || X <- Merged].

spn_do_not_change_players_order(_) ->
    S1 = bs_server:get_session(test),
    Players1 = S1#bridge_session.players,
    Players2 = bs_server:set_player_name(test, north, "NorthName"), 
    Players3 = bs_server:set_player_name(test, south, "SouthName"), 
    Players4 = bs_server:set_player_name(test, west, "WestName"), 
    Players5 = bs_server:set_player_name(test, east, "EastName"), 
    [
        has_the_same_order(Players1, Players2),
        has_the_same_order(Players2, Players3),
        has_the_same_order(Players3, Players4),
        has_the_same_order(Players4, Players5)
    ].

has_the_same_order(L1, L2) ->
    [ ?_assertEqual(P1#player.id, P2#player.id) || {P1, P2} <- lists:zip(L1, L2)].

%% ============================================================================================================
%%  Tests for function switch_players
%% ============================================================================================================
% should affect only positions of two players
sp_affects_only_positions_of_two_players(_) ->
    S1 = bs_server:get_session(test),
    Players1 = S1#bridge_session.players,
    Players2 = bs_server:switch_players(test, north, south), 
    Players3 = bs_server:switch_players(test, north, west), 
    Players4 = bs_server:switch_players(test, north, east), 
    Players5 = bs_server:switch_players(test, south, east), 
    Players6 = bs_server:switch_players(test, south, west), 
    Players7 = bs_server:switch_players(test, east, west), 
    [
        are_two_players_switched(Players1, Players2, north, south),
        are_two_players_switched(Players2, Players3, north, west),
        are_two_players_switched(Players3, Players4, north, east),
        are_two_players_switched(Players4, Players5, south, east),
        are_two_players_switched(Players5, Players6, south, west),
        are_two_players_switched(Players6, Players7, east, west)
    ].

are_two_players_switched(Before, After, Pos1, Pos2) ->
    Merged = merge_the_same_players(Before, After),
    CheckPlayer = fun({B,A}) -> 
        case B#player.position of
            Pos1 ->
                ?_assertEqual(Pos2, A#player.position);
            Pos2 ->
                ?_assertEqual(Pos1, A#player.position);
            Other ->
                ?_assertEqual(Other, A#player.position)
        end
    end,
    [CheckPlayer(X) || X <- Merged].

merge_the_same_players(L1, L2) ->
    Match = fun(X) ->
            [M] = lists:filter(fun(Y) -> Y#player.id == X#player.id end, L2),
            M
    end,
    [ {P, Match(P)} || P <- L1].

% after invokation all positions should be covered
sp_are_all_positions_covered_after_invocation(_) ->
    S1 = bs_server:get_session(test),
    Players1 = S1#bridge_session.players,
    Players2 = bs_server:switch_players(test, north, south), 
    Players3 = bs_server:switch_players(test, north, west), 
    Players4 = bs_server:switch_players(test, north, east), 
    Players5 = bs_server:switch_players(test, south, east), 
    Players6 = bs_server:switch_players(test, south, west), 
    Players7 = bs_server:switch_players(test, east, west), 
    [
        are_all_positions_covered(Players1),
        are_all_positions_covered(Players2),
        are_all_positions_covered(Players3),
        are_all_positions_covered(Players4),
        are_all_positions_covered(Players5),
        are_all_positions_covered(Players6),
        are_all_positions_covered(Players7)
    ].


