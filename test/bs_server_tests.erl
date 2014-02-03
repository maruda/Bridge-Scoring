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
        {"Get session should return error if given non-atom id",
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
        {"New game should be put in current games of session",
            ?SETUP(fun ng_do_puts_new_game_in_sessions_current_games/1)},
        {"Invoking new game should move previous game to history",
            ?SETUP(fun ng_do_moves_prev_game_to_history/1)},
        {"New game should return error if given non-atom session id",
            ?SETUP(fun ng_are_only_atom_session_ids_accepted/1)},
        {"New game should return error if given non-atom game type",
            ?SETUP(fun ng_are_only_atom_game_types_accepted/1)},
        {"New game should accept only game types that are one of [rubber, sport, imp]",
            ?SETUP(fun ng_are_only_known_game_types_accepted/1)},
        {"New game should throw error on unknown session id",
            ?SETUP(fun ng_is_error_thrown_on_unknown_session_id/1)}
    ].
process_deal_test_() ->
    [
        {"Should return error if given non-atom session id",
            ?SETUP(fun pd_are_only_atom_session_ids_accepted/1)},
        {"Should return error if given non-atom game type",
            ?SETUP(fun pd_are_only_atom_game_types_accepted/1)},
        {"Should accept only game types that are one of [rubber, sport, imp]",
            ?SETUP(fun pd_are_only_known_game_types_accepted/1)},
        {"Should return error on unknown session id",
            ?SETUP(fun pd_is_error_returned_on_unknown_session_id/1)},
        {"Should return error on invalid contract",
            ?SETUP(fun pd_is_error_returned_on_invalid_contract/1)},
        {"Should return error on invalid result",
            ?SETUP(fun pd_is_error_returned_on_invalid_result/1)},
        {"Should affect only game of given type",
            ?SETUP(fun pd_impacts_only_given_game_type/1)},
        {"Should increment round counter",
            ?SETUP(fun pd_increments_round_counter/1)},
        {"Should create new game if processed deal was final for game",
            ?SETUP(fun pd_creates_new_game_after_finishing_one/1)},
        {"Should move closed game to history if created new one",
            ?SETUP(fun pd_moves_closed_game_to_history/1)}
    ].

remove_game_test_() ->
    [
        {"Should return error on non atom session id",
            ?SETUP(fun rg_returns_error_on_non_atom_id/1)},
        {"Should return error on unknown session id",
            ?SETUP(fun rg_returns_error_on_unknown_session_id/1)},
        {"Should not return error on known session id",
            ?SETUP(fun rg_does_not_return_error_on_known_session_id/1)},
        {"Should return removed game",
            ?SETUP(fun rg_should_return_removed_game/1)},
        {"Should return error on unknown game",
            ?SETUP(fun rg_should_return_error_on_unknown_game_id/1)},
        {"Should remove only game wih given id",
            ?SETUP(fun rg_removes_only_game_with_given_id/1)},
        {"Should remove game form current games and from history (depending on where game is)",
            ?SETUP(fun rg_removes_games_regardless_they_are_current_or_in_history/1)},
        {"Removed current game should be replaced with new game",
            ?SETUP(fun rg_creates_new_game_on_removing_current_one/1)},
        {"Removing game from history should remove also players mapped to this game",
            ?SETUP(fun rg_removes_games_from_history_with_players_mapping/1)}
    ].

remove_deal_test_disabled() ->
    [
        {"Should return error on non atom session id",
            ?SETUP(fun rd_returns_error_on_non_atom_id/1)},
        {"Should return error on unknown session id",
            ?SETUP(fun rd_returns_error_on_unknown_session_id/1)},
        {"Should not return error on known session id",
            ?SETUP(fun rd_does_not_return_error_on_known_session_id/1)},
        {"Should return error on unknown game",
            ?SETUP(fun rd_should_return_error_on_unknown_game_id/1)},
        {"Should not return unknown_game_error_for_known_game",
            ?SETUP(fun rd_does_not_return_unknown_game_error_for_known_game/1)},
        {"Only last deal may be removed",
            ?SETUP(fun rd_restores_previous_state/1)},
        {"Games in history are unknown for this function",
            ?SETUP(fun rd_takes_games_in_history_as_unknown/1)},
        {"Should decrement round counter",
            ?SETUP(fun rd_decrements_round_counter/1)}
    ].

set_player_name_test_() ->
    [
        {"Should return error on non atom session id",
            ?SETUP(fun spn_returns_error_on_non_atom_id/1)},
        {"Should return error on unknown session id",
            ?SETUP(fun spn_returns_error_on_unknown_session_id/1)},
        {"Should not return error on known session id",
            ?SETUP(fun spn_does_not_return_error_on_known_session_id/1)},
        {"Should accept only valid positions",
            ?SETUP(fun spn_accepts_only_valid_positions/1)},
        {"Should accept only string and atom names",
            ?SETUP(fun spn_accepts_only_string_and_atom_names/1)},
        {"Should change players name",
            ?SETUP(fun spn_changes_player_name/1)},
        {"Should affect only player with given position",
            ?SETUP(fun spn_affects_only_player_with_given_position/1)},
        {"Should not change players order",
            ?SETUP(fun spn_do_not_change_players_order/1)}
    ].

switch_players_test_() ->
    [
        {"Should return error on non atom session id",
            ?SETUP(fun sp_returns_error_on_non_atom_id/1)},
        {"Should return error on unknown session id",
            ?SETUP(fun sp_returns_error_on_unknown_session_id/1)},
        {"Should not return error on known session id",
            ?SETUP(fun sp_does_not_return_error_on_known_session_id/1)},
        {"Should accept only valid positions",
            ?SETUP(fun sp_accepts_only_valid_positions/1)},
        {"Shound not make chnges when the same positions are passed as params",
            ?SETUP(fun sp_does_not_change_anything_on_switching_same_positions/1)},
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
    S1 = bs_server:get_session(test),
    S2 = bs_server:new_game(test, rubber),
    S3 = bs_server:new_game(test, sport),
    S4 = bs_server:new_game(test, imp),
    [
        % history check
        ?_assertEqual(S1#bridge_session.history#history.sport, S2#bridge_session.history#history.sport),
        ?_assertEqual(S1#bridge_session.history#history.imp, S2#bridge_session.history#history.imp),
        ?_assertEqual(S2#bridge_session.history#history.rubber, S3#bridge_session.history#history.rubber),
        ?_assertEqual(S2#bridge_session.history#history.imp, S3#bridge_session.history#history.imp),
        ?_assertEqual(S3#bridge_session.history#history.rubber, S4#bridge_session.history#history.rubber),
        ?_assertEqual(S3#bridge_session.history#history.sport, S4#bridge_session.history#history.sport),
        ?_assertNotEqual(S1#bridge_session.history#history.rubber, S2#bridge_session.history#history.rubber),
        ?_assertNotEqual(S2#bridge_session.history#history.sport, S3#bridge_session.history#history.sport),
        ?_assertNotEqual(S3#bridge_session.history#history.imp, S4#bridge_session.history#history.imp),
        % current games check
        generate_current_game_check_tests(S1#bridge_session.games_states, S2#bridge_session.games_states, rubber),
        generate_current_game_check_tests(S2#bridge_session.games_states, S3#bridge_session.games_states, sport),
        generate_current_game_check_tests(S3#bridge_session.games_states, S4#bridge_session.games_states, imp)
    ].

merge_games_of_same_type(L1, L2) ->
    Match = fun(X) -> 
            [{_, M}] = lists:filter(fun({_, Y}) -> Y#game_state.game_type == X#game_state.game_type end, L2),
            M
    end,
    [{X, Match(X)} || {_, X} <- L1].

generate_current_game_check_tests(L1, L2, Type) ->
    lists:map(fun({X, Y}) -> 
                case X#game_state.game_type of
                    Type -> ?_assertNotEqual(X,Y);
                    _ -> ?_assertEqual(X, Y)
                end
        end, merge_games_of_same_type(L1, L2)).

ng_do_puts_new_game_in_sessions_current_games(_) ->
    S1 = bs_server:get_session(test),
    _S2 = bs_server:new_game(test, rubber),
    _S3 = bs_server:new_game(test, sport),
    S4 = bs_server:new_game(test, imp),
    [ ?_assertNotEqual(X,Y) || {X,Y} <- merge_games_of_same_type(S1#bridge_session.games_states, S4#bridge_session.games_states)].

ng_do_moves_prev_game_to_history(_) ->
    BS = bs_server:get_session(test),
    P = BS#bridge_session.players,
    [{_, BRubber}] = lists:filter(fun({_T, X}) -> X#game_state.game_type==rubber end, BS#bridge_session.games_states),
    [{_, BSport}] = lists:filter(fun({_T, X}) -> X#game_state.game_type==sport end, BS#bridge_session.games_states),
    [{_, BImp}] = lists:filter(fun({_T, X}) -> X#game_state.game_type==imp end, BS#bridge_session.games_states),
    bs_server:new_game(test, rubber),
    bs_server:new_game(test, sport),
    bs_server:new_game(test, imp),
    S = bs_server:get_session(test),
    [
        ?_assert(lists:member({BRubber, P}, S#bridge_session.history#history.rubber)),
        ?_assert(lists:member({BSport, P}, S#bridge_session.history#history.sport)),
        ?_assert(lists:member({BImp, P}, S#bridge_session.history#history.imp))
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
pd_are_only_atom_session_ids_accepted(_) ->
    [
        ?_assertEqual({badarg, non_atom_id}, bs_server:process_deal("string", rubber, #contract{}, #result{})),
        ?_assertEqual({badarg, non_atom_id}, bs_server:process_deal(<<"binary_string">>, rubber, #contract{}, #result{})),
        ?_assertEqual({badarg, non_atom_id}, bs_server:process_deal(1234, rubber, #contract{}, #result{})),
        ?_assertEqual({badarg, non_atom_id}, bs_server:process_deal(self(), rubber, #contract{}, #result{})),
        ?_assertEqual({badarg, non_atom_id}, bs_server:process_deal([list, 'of', atoms], rubber, #contract{}, #result{})),
        ?_assertEqual({badarg, non_atom_id}, bs_server:process_deal({tuple, 'of', 4, "terms"}, rubber, #contract{}, #result{}))
    ].

pd_are_only_atom_game_types_accepted(_) ->
    bs_server:get_session(id),
    [
        ?_assertEqual({badarg, non_atom_game_type}, bs_server:process_deal(id, "string", #contract{}, #result{})),
        ?_assertEqual({badarg, non_atom_game_type}, bs_server:process_deal(id, <<"binary string">>, #contract{}, #result{})),
        ?_assertEqual({badarg, non_atom_game_type}, bs_server:process_deal(id, 1234, #contract{}, #result{})),
        ?_assertEqual({badarg, non_atom_game_type}, bs_server:process_deal(id, self(), #contract{}, #result{})),
        ?_assertEqual({badarg, non_atom_game_type}, bs_server:process_deal(id, [list, "of", 4, terms], #contract{}, #result{})),
        ?_assertEqual({badarg, non_atom_game_type}, bs_server:process_deal(id, {"tuple of", something}, #contract{}, #result{})),
        % check valid values
        ?_assertNotEqual({badarg, non_atom_game_type}, bs_server:process_deal(id, rubber, #contract{}, #result{})),
        ?_assertNotEqual({badarg, non_atom_game_type}, bs_server:process_deal(id, sport, #contract{}, #result{})),
        ?_assertNotEqual({badarg, non_atom_game_type}, bs_server:process_deal(id, imp, #contract{}, #result{}))
    ].

pd_are_only_known_game_types_accepted(_) ->
    [
        ?_assertEqual({badarg, unknown_game_type}, bs_server:process_deal(test, unknown, #contract{}, #result{})),
        ?_assertEqual({badarg, unknown_game_type}, bs_server:process_deal(test, undefinedi, #contract{}, #result{})),
        ?_assertEqual({badarg, unknown_game_type}, bs_server:process_deal(test, any_atom, #contract{}, #result{})),
        ?_assertNotEqual({badarg, unknown_game_type}, bs_server:process_deal(test, rubber, #contract{}, #result{})),
        ?_assertNotEqual({badarg, unknown_game_type}, bs_server:process_deal(test, sport, #contract{}, #result{})),
        ?_assertNotEqual({badarg, unknown_game_type}, bs_server:process_deal(test, imp, #contract{}, #result{}))
    ].

pd_is_error_returned_on_unknown_session_id(_) ->
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% bs_server is started once for all tests           %%
    %% using the same session id as used in other tests  %%
    %% may make this test faling as id is                %%
    %% in fact already known, so beware :)               %%
    %% (other tests uses session ids: 'id' and 'test'    %%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    [
        ?_assertEqual({badarg, unknown_session}, bs_server:process_deal(pd_session_id, rubber, #contract{}, #result{})),
        ?_assertEqual({badarg, unknown_session}, bs_server:process_deal(pd_test_id, rubber, #contract{}, #result{})),
        ?_assertEqual({badarg, unknown_session}, bs_server:process_deal(pd_undefined, rubber, #contract{}, #result{})),
        ?_assertEqual({badarg, unknown_session}, bs_server:process_deal(pd_unknown, rubber, #contract{}, #result{}))
    ].

pd_is_error_returned_on_invalid_contract(_) ->
    [
        ?_assertEqual({badarg, invalid_contract}, bs_server:process_deal(test, rubber, #contract{level=0}, #result{})),
        ?_assertEqual({badarg, invalid_contract}, bs_server:process_deal(test, rubber, #contract{level=8}, #result{})),
        ?_assertEqual({badarg, invalid_contract}, bs_server:process_deal(test, rubber, #contract{level=-2}, #result{})),
        ?_assertEqual({badarg, invalid_contract}, bs_server:process_deal(test, rubber, #contract{level="string"}, #result{})),
        ?_assertEqual({badarg, invalid_contract}, bs_server:process_deal(test, rubber, #contract{level=atom}, #result{})),
        ?_assertEqual({badarg, invalid_contract}, bs_server:process_deal(test, rubber, #contract{level= <<"binary">>}, #result{})),
        ?_assertEqual({badarg, invalid_contract}, bs_server:process_deal(test, rubber, #contract{level=self()}, #result{})),
        ?_assertNotEqual({badarg, invalid_contract}, bs_server:process_deal(test, rubber, #contract{level=1}, #result{}))
    ].

pd_is_error_returned_on_invalid_result(_) ->
    [
        ?_assertEqual({badarg, invalid_result}, bs_server:process_deal(test, rubber, #contract{}, #result{taken=-1})),
        ?_assertEqual({badarg, invalid_result}, bs_server:process_deal(test, rubber, #contract{}, #result{taken=14})),
        ?_assertEqual({badarg, invalid_result}, bs_server:process_deal(test, rubber, #contract{}, #result{taken=atom})),
        ?_assertEqual({badarg, invalid_result}, bs_server:process_deal(test, rubber, #contract{}, #result{taken="string"})),
        ?_assertEqual({badarg, invalid_result}, bs_server:process_deal(test, rubber, #contract{}, #result{taken= <<"binary">>})),
        ?_assertEqual({badarg, invalid_result}, bs_server:process_deal(test, rubber, #contract{}, #result{miltons=-1})),
        ?_assertEqual({badarg, invalid_result}, bs_server:process_deal(test, rubber, #contract{}, #result{miltons=41})),
        ?_assertEqual({badarg, invalid_result}, bs_server:process_deal(test, rubber, #contract{}, #result{miltons=atom})),
        ?_assertEqual({badarg, invalid_result}, bs_server:process_deal(test, rubber, #contract{}, #result{miltons="string"})),
        ?_assertEqual({badarg, invalid_result}, bs_server:process_deal(test, rubber, #contract{}, #result{miltons= <<"binary">>}))
    ].

pd_impacts_only_given_game_type(_) ->
    BS = bs_server:get_session(test),
    [{_, BRubber}] = lists:filter(fun({_T, X}) -> X#game_state.game_type==rubber end, BS#bridge_session.games_states),
    [{_, BSport}] = lists:filter(fun({_T, X}) -> X#game_state.game_type==sport end, BS#bridge_session.games_states),
    [{_, BImp}] = lists:filter(fun({_T, X}) -> X#game_state.game_type==imp end, BS#bridge_session.games_states),
    bs_server:process_deal(test, rubber, #contract{}, #result{}),
    AS = bs_server:get_session(test),
    [{_, ARubber}] = lists:filter(fun({_T, X}) -> X#game_state.game_type==rubber end, AS#bridge_session.games_states),
    [{_, ASport}] = lists:filter(fun({_T, X}) -> X#game_state.game_type==sport end, AS#bridge_session.games_states),
    [{_, AImp}] = lists:filter(fun({_T, X}) -> X#game_state.game_type==imp end, AS#bridge_session.games_states),
    [
        ?_assertEqual(ASport, BSport), 
        ?_assertEqual(AImp, BImp), 
        ?_assertNotEqual(ARubber, BRubber)
    ].

pd_increments_round_counter(_) ->
    bs_server:get_session(test),
    GS1 = bs_server:process_deal(test, rubber, #contract{}, #result{}),
    GS2 = bs_server:process_deal(test, rubber, #contract{}, #result{}),
    GS3 = bs_server:process_deal(test, rubber, #contract{}, #result{}),
    GS4 = bs_server:process_deal(test, rubber, #contract{}, #result{}),
    GS5 = bs_server:process_deal(test, rubber, #contract{}, #result{}),
    [
        ?_assertEqual(GS1#game_state.round_no + 1, GS2#game_state.round_no),
        ?_assertEqual(GS2#game_state.round_no + 1, GS3#game_state.round_no),
        ?_assertEqual(GS3#game_state.round_no + 1, GS4#game_state.round_no),
        ?_assertEqual(GS4#game_state.round_no + 1, GS5#game_state.round_no)
    ].

pd_creates_new_game_after_finishing_one(_) ->
    bs_server:get_session(test),
    bs_server:new_game(test, rubber),
    WinningContract = #contract{level=7},
    Result = #result{taken=13},
    bs_server:process_deal(test, rubber, WinningContract, Result),
    bs_server:process_deal(test, rubber, WinningContract, Result),
    BS = bs_server:get_session(test),
    [{_, CurrentGame}] = lists:filter(fun({_T, X}) -> X#game_state.game_type==rubber end, BS#bridge_session.games_states),
    bs_server:new_game(test, rubber),
    [{_, NewGame}] = lists:filter(fun({_T, X}) -> X#game_state.game_type==rubber end, BS#bridge_session.games_states),
    % afer overriding id NewGame and CurrentGame should be equal
    NewGame#game_state{game_id=CurrentGame#game_state.game_id},
    [
        ?_assertEqual(CurrentGame, NewGame)
    ].

pd_moves_closed_game_to_history(_) ->
    bs_server:get_session(test),
    bs_server:new_game(test, rubber),
    WinningContract = #contract{level=7},
    Result = #result{taken=13},
    bs_server:process_deal(test, rubber, WinningContract, Result),
    GS = bs_server:process_deal(test, rubber, WinningContract, Result),
    BS = bs_server:get_session(test),
    P = BS#bridge_session.players,
    [
        ?_assert(lists:member({GS, P}, BS#bridge_session.history#history.rubber))
    ].

%% ============================================================================================================
%%  Tests for function remove_game
%% ============================================================================================================
rg_returns_error_on_non_atom_id(_) ->
    [
        ?_assertEqual({badarg, non_atom_id}, bs_server:remove_game("string", game_id)), 
        ?_assertEqual({badarg, non_atom_id}, bs_server:remove_game(<<"binary_string">>, game_id)), 
        ?_assertEqual({badarg, non_atom_id}, bs_server:remove_game(1234, game_id)), 
        ?_assertEqual({badarg, non_atom_id}, bs_server:remove_game(self(), game_id)), 
        ?_assertEqual({badarg, non_atom_id}, bs_server:remove_game([list, 'of', atoms], game_id)),  
        ?_assertEqual({badarg, non_atom_id}, bs_server:remove_game({tuple, 'of', 4, "terms"}, game_id)),
        ?_assertEqual({badarg, non_atom_id}, bs_server:remove_game(session_id, "string")), 
        ?_assertEqual({badarg, non_atom_id}, bs_server:remove_game(session_id, <<"binary_string">>)), 
        ?_assertEqual({badarg, non_atom_id}, bs_server:remove_game(session_id, 1234)), 
        ?_assertEqual({badarg, non_atom_id}, bs_server:remove_game(session_id, self())), 
        ?_assertEqual({badarg, non_atom_id}, bs_server:remove_game(session_id, [list, 'of', atoms])),  
        ?_assertEqual({badarg, non_atom_id}, bs_server:remove_game(session_id, {tuple, 'of', 4, "terms"}))
    ].

rg_returns_error_on_unknown_session_id(_) ->
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% bs_server is started once for all tests           %%
    %% using the same session id as used in other tests  %%
    %% may make this test faling as id is                %%
    %% in fact already known, so beware :)               %%
    %% (other tests uses session ids: 'id' and 'test'    %%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    [
        ?_assertEqual({badarg, unknown_session}, bs_server:remove_game(rg_session_id, game_id)),
        ?_assertEqual({badarg, unknown_session}, bs_server:remove_game(rg_test_id, game_id)),
        ?_assertEqual({badarg, unknown_session}, bs_server:remove_game(rg_undefined, game_id)),
        ?_assertEqual({badarg, unknown_session}, bs_server:remove_game(rd_unknown, game_id))
    ].

rg_does_not_return_error_on_known_session_id(_) ->
    bs_server:get_session(id), % <- it is known right now
    bs_server:get_session(test), % <- it is known right now
    bs_server:get_session(undefined), % <- it is known right now
    bs_server:get_session(unknown), % <- it is known right now
    [
        ?_assertNotEqual({badarg, unknown_session}, bs_server:remove_game(id, id)),
        ?_assertNotEqual({badarg, unknown_session}, bs_server:remove_game(test, id)),
        ?_assertNotEqual({badarg, unknown_session}, bs_server:remove_game(undefined, id)),
        ?_assertNotEqual({badarg, unknown_session}, bs_server:remove_game(unknown, id))
    ].

rg_should_return_removed_game(_) ->
    [
        % TODO
    ].

rg_should_return_error_on_unknown_game_id(_) ->
    [
    ].

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
rd_returns_error_on_non_atom_id(_) ->
    [
        ?_assertEqual({badarg, non_atom_id}, bs_server:remove_deal("string", game_id)), 
        ?_assertEqual({badarg, non_atom_id}, bs_server:remove_deal(<<"binary_string">>, game_id)), 
        ?_assertEqual({badarg, non_atom_id}, bs_server:remove_deal(1234, game_id)), 
        ?_assertEqual({badarg, non_atom_id}, bs_server:remove_deal(self(), game_id)), 
        ?_assertEqual({badarg, non_atom_id}, bs_server:remove_deal(["list", 'of', 4, <<"things">>], game_id)), 
        ?_assertEqual({badarg, non_atom_id}, bs_server:remove_deal({tuple, "of", 3},  game_id)), 
        ?_assertEqual({badarg, non_atom_id}, bs_server:remove_deal(session_id, "string")), 
        ?_assertEqual({badarg, non_atom_id}, bs_server:remove_deal(session_id, <<"binary_string">>)),
        ?_assertEqual({badarg, non_atom_id}, bs_server:remove_deal(session_id, 1234)),
        ?_assertEqual({badarg, non_atom_id}, bs_server:remove_deal(session_id, self())),
        ?_assertEqual({badarg, non_atom_id}, bs_server:remove_deal(session_id, ["list", 'of', 4, <<"things">>])),
        ?_assertEqual({badarg, non_atom_id}, bs_server:remove_deal(session_id, {tuple, "of", 3}))
    ].

rd_returns_error_on_unknown_session_id(_) ->
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% bs_server is started once for all tests           %%
    %% using the same session id as used in other tests  %%
    %% may make this test faling as id is                %%
    %% in fact already known, so beware :)               %%
    %% (other tests uses session ids: 'id' and 'test'    %%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    [
        ?_assertEqual({badarg, unknown_session}, bs_server:remove_deal(rd_session_id, game_id)),
        ?_assertEqual({badarg, unknown_session}, bs_server:remove_deal(rd_test_id, game_id)),
        ?_assertEqual({badarg, unknown_session}, bs_server:remove_deal(rd_undefined, game_id)),
        ?_assertEqual({badarg, unknown_session}, bs_server:remove_deal(rd_unknown, game_id))
    ].

rd_should_return_error_on_unknown_game_id(_) ->
    bs_server:get_session(test),
    [
        ?_assertEqual({badarg, unknown_game}, bs_server:remove_deal(test, unknown_game_id))
    ].

rd_does_not_return_unknown_game_error_for_known_game(_) ->
    S = bs_server:get_session(test),
    GIds = [ X#game_state.game_id || {_, X} <- S#bridge_session.games_states ],
    [
        [ ?_assertNotEqual({badarg, unknown_game_id}, bs_server:remove_deal(test, Gid)) || Gid <- GIds ]
    ].

rd_does_not_return_error_on_known_session_id(_) ->
    bs_server:get_session(id), % <- it is known right now
    bs_server:get_session(test), % <- it is known right now
    bs_server:get_session(undefined), % <- it is known right now
    bs_server:get_session(unknown), % <- it is known right now
    [
        ?_assertNotEqual({badarg, unknown_session}, bs_server:remove_deal(id, id)),
        ?_assertNotEqual({badarg, unknown_session}, bs_server:remove_deal(test, id)),
        ?_assertNotEqual({badarg, unknown_session}, bs_server:remove_deal(undefined, id)),
        ?_assertNotEqual({badarg, unknown_session}, bs_server:remove_deal(unknown, id))
    ].

rd_restores_previous_state(_) ->
    % TODO similar test for other game types
    bs_server:get_session(test),
    S1 = bs_server:new_game(test, rubber),
    [{_, GS1}] = lists:filter(fun({_T, X}) -> X#game_state.game_type==rubber end, S1#bridge_session.games_states),
    GS2 = bs_server:process_deal(test, rubber, #contract{}, #result{}),
    GS3 = bs_server:process_deal(test, rubber, #contract{level=3}, #result{taken=9}),
    GS4 = bs_server:process_deal(test, rubber, #contract{level=3}, #result{taken=7}),
    GS5 = bs_server:process_deal(test, rubber, #contract{level=2}, #result{taken=8}),
    %% 'NS' is vulnerable now
    GS6 = bs_server:process_deal(test, rubber, #contract{owner='WE', level=2}, #result{taken=10}),
    GS7 = bs_server:process_deal(test, rubber, #contract{owner='WE', level=6}, #result{taken=10}),
    GS8 = bs_server:process_deal(test, rubber, #contract{owner='WE', level=4}, #result{taken=10}),
    %% 'WE' is vulnerable now
    GS9 = bs_server:process_deal(test, rubber, #contract{owner='NS', level=4}, #result{taken=10}),
    GS10 = bs_server:process_deal(test, rubber, #contract{owner='WE', level=4}, #result{taken=10}),
    Gid = GS10#game_state.game_id,
    [
        ?_assertEqual(GS9, bs_server:remove_deal(test, Gid)),
        ?_assertEqual(GS8, bs_server:remove_deal(test, Gid)),
        ?_assertEqual(GS7, bs_server:remove_deal(test, Gid)),
        ?_assertEqual(GS6, bs_server:remove_deal(test, Gid)),
        ?_assertEqual(GS5, bs_server:remove_deal(test, Gid)),
        ?_assertEqual(GS4, bs_server:remove_deal(test, Gid)),
        ?_assertEqual(GS3, bs_server:remove_deal(test, Gid)),
        ?_assertEqual(GS2, bs_server:remove_deal(test, Gid)),
        ?_assertEqual(GS1, bs_server:remove_deal(test, Gid))
    ].

rd_takes_games_in_history_as_unknown(_) ->
    bs_server:get_session(test),
    bs_server:new_game(test, rubber),
    bs_server:new_game(test, rubber),
    bs_server:new_game(test, sport),
    bs_server:new_game(test, imp),
    S = bs_server:new_game(test, imp),
    GIds1 = [ X#game_state.game_id || {X, _} <- S#bridge_session.history#history.rubber ],
    GIds2 = [ X#game_state.game_id || {X, _} <- S#bridge_session.history#history.sport ],
    GIds3 = [ X#game_state.game_id || {X, _} <- S#bridge_session.history#history.imp ],
    [
        [ ?_assertEqual({badarg, unknown_game_id}, bs_server:remove_deal(test, Gid)) || Gid <- GIds1 ],
        [ ?_assertEqual({badarg, unknown_game_id}, bs_server:remove_deal(test, Gid)) || Gid <- GIds2 ],
        [ ?_assertEqual({badarg, unknown_game_id}, bs_server:remove_deal(test, Gid)) || Gid <- GIds3 ]
    ].


rd_decrements_round_counter(_) ->
    bs_server:get_session(test),
    GS1 = bs_server:process_deal(test, rubber, #contract{}, #result{}),
    GS2 = bs_server:process_deal(test, rubber, #contract{}, #result{}),
    GS3 = bs_server:process_deal(test, rubber, #contract{}, #result{}),
    GS4 = bs_server:process_deal(test, rubber, #contract{}, #result{}),
    GS5 = bs_server:process_deal(test, rubber, #contract{}, #result{}),
    R1 = bs_server:remove_deal(test, GS1#game_state.game_id),
    R2 = bs_server:remove_deal(test, GS1#game_state.game_id),
    R3 = bs_server:remove_deal(test, GS1#game_state.game_id),
    R4 = bs_server:remove_deal(test, GS1#game_state.game_id),
    [
        ?_assertEqual(R1#game_state.round_no, GS5#game_state.round_no - 1),
        ?_assertEqual(R2#game_state.round_no, R1#game_state.round_no - 1),
        ?_assertEqual(R3#game_state.round_no, R2#game_state.round_no - 1),
        ?_assertEqual(R4#game_state.round_no, R3#game_state.round_no - 1),
        ?_assertEqual(R1#game_state.round_no, GS4#game_state.round_no),
        ?_assertEqual(R2#game_state.round_no, GS3#game_state.round_no),
        ?_assertEqual(R3#game_state.round_no, GS2#game_state.round_no),
        ?_assertEqual(R4#game_state.round_no, GS1#game_state.round_no)
    ].

%% ============================================================================================================
%%  Tests for function set_player_name
%% ============================================================================================================

spn_returns_error_on_non_atom_id(_) ->
    [
        ?_assertEqual({badarg, non_atom_id}, bs_server:set_player_name("string", north, south)),
        ?_assertEqual({badarg, non_atom_id}, bs_server:set_player_name(<<"binary_string">>, north, south)),
        ?_assertEqual({badarg, non_atom_id}, bs_server:set_player_name(1234, north, south)),
        ?_assertEqual({badarg, non_atom_id}, bs_server:set_player_name(self(), north, south)),
        ?_assertEqual({badarg, non_atom_id}, bs_server:set_player_name(["list", 'of', 4, <<"things">>], north, south)),
        ?_assertEqual({badarg, non_atom_id}, bs_server:set_player_name({tuple, "of", 3}, north, south))
    ].

spn_returns_error_on_unknown_session_id(_) ->
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% bs_server is started once for all tests           %%
    %% using the same session id as used in other tests  %%
    %% may make this test faling as id is                %%
    %% in fact already known, so beware :)               %%
    %% (other tests uses session ids: 'id' and 'test'    %%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    [
        ?_assertEqual({badarg, unknown_session}, bs_server:set_player_name(spn_session_id, north, south)),
        ?_assertEqual({badarg, unknown_session}, bs_server:set_player_name(spn_test_id, east, south)),
        ?_assertEqual({badarg, unknown_session}, bs_server:set_player_name(spn_undefined, east, west)),
        ?_assertEqual({badarg, unknown_session}, bs_server:set_player_name(spn_unknown, east, north))
    ].

spn_does_not_return_error_on_known_session_id(_) ->
    bs_server:get_session(id), % <- it is known right now
    bs_server:get_session(test), % <- it is known right now
    bs_server:get_session(undefined), % <- it is known right now
    bs_server:get_session(unknown), % <- it is known right now
    [
        ?_assertNotEqual({badarg, unknown_session}, bs_server:set_player_name(id, north, south)),
        ?_assertNotEqual({badarg, unknown_session}, bs_server:set_player_name(test, east, south)),
        ?_assertNotEqual({badarg, unknown_session}, bs_server:set_player_name(undefined, east, west)),
        ?_assertNotEqual({badarg, unknown_session}, bs_server:set_player_name(unknown, east, north))
    ].

spn_accepts_only_valid_positions(_) ->
    [
        ?_assertEqual({badarg, invalid_position}, bs_server:set_player_name(id, test, atom)),
        ?_assertEqual({badarg, invalid_position}, bs_server:set_player_name(id, "bad", "types")),
        ?_assertEqual({badarg, invalid_position}, bs_server:set_player_name(id, <<"bin">>, atom)),
        ?_assertEqual({badarg, invalid_position}, bs_server:set_player_name(id, test, "Name")),
        ?_assertEqual({badarg, invalid_position}, bs_server:set_player_name(id, self(), atom)),
        ?_assertEqual({badarg, invalid_position}, bs_server:set_player_name(id, test, [list, "of", 3])),
        ?_assertEqual({badarg, invalid_position}, bs_server:set_player_name(id, {"tuple", 'of', 4, <<"terms">>}, atom)),
        ?_assertEqual({badarg, invalid_position}, bs_server:set_player_name(id, "north", "south")),
        ?_assertEqual({badarg, invalid_position}, bs_server:set_player_name(id, "west", "east"))
    ].

spn_accepts_only_string_and_atom_names(_) ->
    [
        ?_assertEqual({badarg, invalid_name}, bs_server:set_player_name(id, north, 1234)),
        ?_assertEqual({badarg, invalid_name}, bs_server:set_player_name(id, south, self())),
        ?_assertEqual({badarg, invalid_name}, bs_server:set_player_name(id, west, {tuple, "of", <<"terms">>})),
        ?_assertNotEqual({badarg, invalid_name}, bs_server:set_player_name(id, north, "Name")),
        ?_assertNotEqual({badarg, invalid_name}, bs_server:set_player_name(id, north, name)),
        ?_assertNotEqual({badarg, invalid_name}, bs_server:set_player_name(id, north, 'Name')),
        ?_assertNotEqual({badarg, invalid_name}, bs_server:set_player_name(id, north, [68,72,84,92]))
    ].

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

sp_returns_error_on_non_atom_id(_) ->
    [
        ?_assertEqual({badarg, non_atom_id}, bs_server:switch_players("string", north, south)),
        ?_assertEqual({badarg, non_atom_id}, bs_server:switch_players(<<"binary_string">>, north, south)),
        ?_assertEqual({badarg, non_atom_id}, bs_server:switch_players(1234, north, south)),
        ?_assertEqual({badarg, non_atom_id}, bs_server:switch_players(self(), north, south)),
        ?_assertEqual({badarg, non_atom_id}, bs_server:switch_players(["list", 'of', 4, <<"things">>], north, south)),
        ?_assertEqual({badarg, non_atom_id}, bs_server:switch_players({tuple, "of", 3}, north, south))
    ].

sp_returns_error_on_unknown_session_id(_) ->
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% bs_server is started once for all tests           %%
    %% using the same session id as used in other tests  %%
    %% may make this test faling as id is                %%
    %% in fact already known, so beware :)               %%
    %% (other tests uses session ids: 'id' and 'test'    %%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    [
        ?_assertEqual({badarg, unknown_session}, bs_server:switch_players(sp_session_id, north, south)),
        ?_assertEqual({badarg, unknown_session}, bs_server:switch_players(sp_test_id, east, south)),
        ?_assertEqual({badarg, unknown_session}, bs_server:switch_players(sp_undefined, east, west)),
        ?_assertEqual({badarg, unknown_session}, bs_server:switch_players(sp_unknown, east, north))
    ].

sp_does_not_return_error_on_known_session_id(_) ->
    bs_server:get_session(id),
    bs_server:get_session(test),
    bs_server:get_session(undefined),
    bs_server:get_session(unknown), % <- it is known right now
    [
        ?_assertNotEqual({badarg, unknown_session}, bs_server:switch_players(id, north, south)),
        ?_assertNotEqual({badarg, unknown_session}, bs_server:switch_players(test, east, south)),
        ?_assertNotEqual({badarg, unknown_session}, bs_server:switch_players(undefined, east, west)),
        ?_assertNotEqual({badarg, unknown_session}, bs_server:switch_players(unknown, east, north))
    ].

sp_accepts_only_valid_positions(_) ->
    [
        ?_assertEqual({badarg, invalid_position}, bs_server:switch_players(id, test, atom)),
        ?_assertEqual({badarg, invalid_position}, bs_server:switch_players(id, "bad", "types")),
        ?_assertEqual({badarg, invalid_position}, bs_server:switch_players(id, <<"bin">>, atom)),
        ?_assertEqual({badarg, invalid_position}, bs_server:switch_players(id, test, 1234)),
        ?_assertEqual({badarg, invalid_position}, bs_server:switch_players(id, self(), atom)),
        ?_assertEqual({badarg, invalid_position}, bs_server:switch_players(id, test, [list, "of", 3])),
        ?_assertEqual({badarg, invalid_position}, bs_server:switch_players(id, {"tuple", 'of', 4, <<"terms">>}, atom)),
        ?_assertEqual({badarg, invalid_position}, bs_server:switch_players(id, "north", "south")),
        ?_assertEqual({badarg, invalid_position}, bs_server:switch_players(id, "west", "east"))
    ].
% switching same positions should not change anything
sp_does_not_change_anything_on_switching_same_positions(_) ->
    S1 = bs_server:get_session(test),
    Players1 = S1#bridge_session.players,
    Players2 = bs_server:switch_players(test, north, north), 
    Players3 = bs_server:switch_players(test, west, west), 
    Players4 = bs_server:switch_players(test, east, east), 
    Players5 = bs_server:switch_players(test, south, south), 
    [
        ?_assertEqual(Players1, Players2),
        ?_assertEqual(Players2, Players3),
        ?_assertEqual(Players3, Players4),
        ?_assertEqual(Players4, Players5)
    ].
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% =================   Utility functions   ============== %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


