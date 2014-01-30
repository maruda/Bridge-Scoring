-module(bs_rubber_score_tests).

%% TODO
%% - implement: bs_rubber_score:create_score_entry_for_bonuses_rubber_won_test
%% - implement: bs_rubber_score:create_score_entry_for_bonuses_extra_points_test

-include_lib("eunit/include/eunit.hrl").
-include("bs_data.hrl").
-include("bs_lang.hrl").

-compile(export_all).

%%-----------------------------------------------------------------------------------------------
%% Tests for function 'process/3'
%%-----------------------------------------------------------------------------------------------
process_simple_test_() ->
    Contract1 = #contract{owner='WE', color='D', level=3},
    Taken1 = 8, % 1 belowtrick
    Contract2 = #contract{owner='NS', color='N', level=3},
    Taken2 = 7, % 2 belowtricks
    Contract3 = #contract{owner='NS', color='H', level=3},
    Taken3 = 9,
    Contract4 = #contract{owner='NS', color='S', level=1},
    Taken4 = 7,
    Contract5 = #contract{owner='WE', color='S', level=4, doubled=true},
    Taken5 = 8, % 2 belowtricks
    Contract6 = #contract{owner='NS', color='H', level=5, doubled=true},
    Taken6 = 10, % 1 belowtrick
    Contract7 = #contract{owner='NS', color='N', level=2},
    Taken7 = 7, % 1 belowtrick
    Contract8 = #contract{owner='WE', color='C', level=4},
    Taken8 = 9, % 1 belowtrick
    Contract9 = #contract{owner='WE', color='N', level=3},
    Taken9 = 9,
    Contract10 = #contract{owner='WE', color='H', level=4},
    Taken10 = 9, % 2 belowtricks
    Contract11 = #contract{owner='NS', color='D', level=5, redoubled=true},
    Taken11 = 9, % 2 belowtricks
    State1 = #game_state{is_NS_vulnerable=false, is_WE_vulnerable=false, round_no=2},
    State2 = State1#game_state{is_NS_vulnerable=true},
    State3 = State2#game_state{is_WE_vulnerable=true},
    TE1 = #score_entry{count=1, 'NS'=[{50, ?COMMENT__POINTS_FOR_OPPONENTS_UNDERTRICKS}], 'WE'=[]},
    TE2 = #score_entry{count=2, 'NS'=[], 'WE'=[{100, ?COMMENT__POINTS_FOR_OPPONENTS_UNDERTRICKS}]},
    TE3 = #score_entry{count=3, 'NS'=[{90, ?COMMENT__POINTS_FOR_CONTRACT_MADE}], 'WE'=[]},
    TE4 = #score_entry{count=4, 'NS'=[{30, ?COMMENT__POINTS_FOR_CONTRACT_MADE}], 'WE'=[]},
    TE5 = #score_entry{count=5, 'NS'=[{300, ?COMMENT__POINTS_FOR_OPPONENTS_UNDERTRICKS}], 'WE'=[]},
    TE6 = #score_entry{count=6, 'NS'=[], 'WE'=[{200, ?COMMENT__POINTS_FOR_OPPONENTS_UNDERTRICKS}]},
    TE7 = #score_entry{count=7, 'NS'=[], 'WE'=[{100, ?COMMENT__POINTS_FOR_OPPONENTS_UNDERTRICKS}]},
    TE8 = #score_entry{count=8, 'NS'=[{50, ?COMMENT__POINTS_FOR_OPPONENTS_UNDERTRICKS}], 'WE'=[]},
    TE9 = #score_entry{count=9, 'NS'=[], 'WE'=[{100, ?COMMENT__POINTS_FOR_CONTRACT_MADE}]},
    TE10 = #score_entry{count=10, 'NS'=[{100, ?COMMENT__POINTS_FOR_OPPONENTS_UNDERTRICKS}], 'WE'=[]},
    TE11 = #score_entry{count=11, 'NS'=[], 'WE'=[{1000, ?COMMENT__POINTS_FOR_OPPONENTS_UNDERTRICKS}]},
    Score0 = #score{above=[], below=[]},
    Score1 = Score0#score{above=[[TE1]]},
    Score2 = Score1#score{above=[[TE1,TE2]]},
    Score3 = Score2#score{below=[TE3]},    
    Score4 = Score3#score{above=[[TE1,TE2],[TE3,TE4]],below=[]},    
    Score5 = Score4#score{above=[[TE1,TE2,TE5],[TE3,TE4]],below=[]},    
    Score6 = Score5#score{above=[[TE1,TE2,TE5,TE6],[TE3,TE4]],below=[]},    
    Score7 = Score6#score{above=[[TE1,TE2,TE5,TE6,TE7],[TE3,TE4]],below=[]},    
    Score8 = Score7#score{above=[[TE1,TE2,TE5,TE6,TE7,TE8],[TE3,TE4]],below=[]},    
    Score9 = Score8#score{above=[[TE1,TE2,TE5,TE6,TE7,TE8],[TE3,TE4],[TE9]],below=[]},    
    Score10 = Score9#score{above=[[TE1,TE2,TE5,TE6,TE7,TE8,TE10],[TE3,TE4],[TE9]],below=[]},    
    Score11 = Score10#score{above=[[TE1,TE2,TE5,TE6,TE7,TE8,TE10,TE11],[TE3,TE4],[TE9]],below=[]},    
    [?_assertEqual(State1#game_state{score=Score1, round_no=2}, bs_rubber_score:process(Contract1, Taken1, State1#game_state{score=Score0, round_no=1})),
    ?_assertEqual(State1#game_state{score=Score2, round_no=3}, bs_rubber_score:process(Contract2, Taken2, State1#game_state{score=Score1, round_no=2})),
    ?_assertEqual(State1#game_state{score=Score3, round_no=4}, bs_rubber_score:process(Contract3, Taken3, State1#game_state{score=Score2, round_no=3})),
    ?_assertEqual(State2#game_state{score=Score4, round_no=5}, bs_rubber_score:process(Contract4, Taken4, State1#game_state{score=Score3, round_no=4})),
    ?_assertEqual(State2#game_state{score=Score5, round_no=6}, bs_rubber_score:process(Contract5, Taken5, State2#game_state{score=Score4, round_no=5})),
    ?_assertEqual(State2#game_state{score=Score6, round_no=7}, bs_rubber_score:process(Contract6, Taken6, State2#game_state{score=Score5, round_no=6})),
    ?_assertEqual(State2#game_state{score=Score7, round_no=8}, bs_rubber_score:process(Contract7, Taken7, State2#game_state{score=Score6, round_no=7})),
    ?_assertEqual(State2#game_state{score=Score8, round_no=9}, bs_rubber_score:process(Contract8, Taken8, State2#game_state{score=Score7, round_no=8})),
    ?_assertEqual(State3#game_state{score=Score9, round_no=10}, bs_rubber_score:process(Contract9, Taken9, State2#game_state{score=Score8, round_no=9})),
    ?_assertEqual(State3#game_state{score=Score10, round_no=11}, bs_rubber_score:process(Contract10, Taken10, State3#game_state{score=Score9, round_no=10})),
    ?_assertEqual(State3#game_state{score=Score11, round_no=12}, bs_rubber_score:process(Contract11, Taken11, State3#game_state{score=Score10, round_no=11}))
    ].

process_more_complex_test_() ->
    Contract1 = #contract{owner='WE', color='D', level=3},
    Taken1 = 11, % 2 overtricks
    Contract2 = #contract{owner='NS', color='N', level=3},
    Taken2 = 8, % 1 belowtricks
    Contract3 = #contract{owner='NS', color='H', level=6},
    Taken3 = 13, % small slam
    Contract4 = #contract{owner='NS', color='S', level=1},
    Taken4 = 7,
    Contract5 = #contract{owner='WE', color='S', level=4, doubled=true},
    Taken5 = 10, % 
    Contract6 = #contract{owner='NS', color='H', level=5, redoubled=true},
    Taken6 = 10, % 1 belowtrick
    Contract7 = #contract{owner='WE', color='N', level=2},
    Taken7 = 8, % 
    Contract8 = #contract{owner='WE', color='C', level=2, doubled=true},
    Taken8 = 9, % 1 overtrick
    State1 = #game_state{is_NS_vulnerable=false, is_WE_vulnerable=false, round_no=1},
    State2 = State1#game_state{is_NS_vulnerable=true},
    State3 = State2#game_state{is_WE_vulnerable=true},
    TE1A = #score_entry{count=1, 'WE'=[{40, ?COMMENT__OVERTRICKS_POINTS}], 'NS'=[]},
    TE1U = #score_entry{count=1, 'WE'=[{60, ?COMMENT__POINTS_FOR_CONTRACT_MADE}], 'NS'=[]},
    TE2 = #score_entry{count=2, 'NS'=[], 'WE'=[{50, ?COMMENT__POINTS_FOR_OPPONENTS_UNDERTRICKS}]},
    TE3A = #score_entry{count=3, 'NS'=[{30, ?COMMENT__OVERTRICKS_POINTS}, {500, ?COMMENT__SMALL_SLAM_BONUS}], 'WE'=[]},
    TE3U = #score_entry{count=3, 'NS'=[{180, ?COMMENT__POINTS_FOR_CONTRACT_MADE}], 'WE'=[]},
    TE4 = #score_entry{count=4, 'NS'=[{30, ?COMMENT__POINTS_FOR_CONTRACT_MADE}], 'WE'=[]},
    TE5A = #score_entry{count=5, 'NS'=[], 'WE'=[{50, ?COMMENT__WON_DOUBLED_GAME}]},
    TE5U = #score_entry{count=5, 'NS'=[], 'WE'=[{240, ?COMMENT__POINTS_FOR_CONTRACT_MADE}]},
    TE6 = #score_entry{count=6, 'NS'=[], 'WE'=[{400, ?COMMENT__POINTS_FOR_OPPONENTS_UNDERTRICKS}]},
    TE7 = #score_entry{count=7, 'NS'=[], 'WE'=[{70, ?COMMENT__POINTS_FOR_CONTRACT_MADE}]},
    TE8A = #score_entry{count=8, 'NS'=[], 'WE'=[{200, ?COMMENT__OVERTRICKS_POINTS}, {50, ?COMMENT__WON_DOUBLED_GAME}, {500, ?COMMENT__WINNING_RUBBER_BONUS}]},
    TE8U = #score_entry{count=8, 'NS'=[], 'WE'=[{80, ?COMMENT__POINTS_FOR_CONTRACT_MADE}]},
    Score0 = #score{above=[], below=[]},
    Score1 = Score0#score{above=[[TE1A]], below=[TE1U]},
    Score2 = Score1#score{above=[[TE1A, TE2]]},
    Score3 = Score2#score{above=[[TE1A, TE2, TE3A], [TE1U, TE3U]], below=[]},    
    Score4 = Score3#score{below=[TE4]},    
    Score5 = Score4#score{above=[[TE1A, TE2, TE3A, TE5A], [TE1U, TE3U], [TE4, TE5U]],below=[]},    
    Score6 = Score5#score{above=[[TE1A, TE2, TE3A, TE5A, TE6], [TE1U, TE3U], [TE4, TE5U]]},    
    Score7 = Score6#score{above=[[TE1A, TE2, TE3A, TE5A, TE6], [TE1U, TE3U], [TE4, TE5U]], below=[TE7]},    
    Score8 = Score7#score{above=[[TE1A, TE2, TE3A, TE5A, TE6, TE8A], [TE1U, TE3U], [TE4, TE5U], [TE7, TE8U]], below=[]},    
    Summary = #summary{winner='WE', ns_score=740, we_score=1740},
    [
    ?_assertEqual(State1#game_state{score=Score1, round_no=2}, bs_rubber_score:process(Contract1, Taken1, State1#game_state{score=Score0, round_no=1})),
    ?_assertEqual(State1#game_state{score=Score2, round_no=3}, bs_rubber_score:process(Contract2, Taken2, State1#game_state{score=Score1, round_no=2})),
    ?_assertEqual(State2#game_state{score=Score3, round_no=4}, bs_rubber_score:process(Contract3, Taken3, State1#game_state{score=Score2, round_no=3})),
    ?_assertEqual(State2#game_state{score=Score4, round_no=5}, bs_rubber_score:process(Contract4, Taken4, State2#game_state{score=Score3, round_no=4})),
    ?_assertEqual(State3#game_state{score=Score5, round_no=6}, bs_rubber_score:process(Contract5, Taken5, State2#game_state{score=Score4, round_no=5})),
    ?_assertEqual(State3#game_state{score=Score6, round_no=7}, bs_rubber_score:process(Contract6, Taken6, State3#game_state{score=Score5, round_no=6})),
    ?_assertEqual(State3#game_state{score=Score7, round_no=8}, bs_rubber_score:process(Contract7, Taken7, State3#game_state{score=Score6, round_no=7})),
    ?_assertEqual(State3#game_state{score=Score8, round_no=9, status=Summary}, bs_rubber_score:process(Contract8, Taken8, State3#game_state{score=Score7, round_no=8}))
    ].


%%--------------------------------------------------------------------------------------------
%% Tests for function 'bs_rubber_score:is_contract_made'
%% test data contains all border cases and few other
%%--------------------------------------------------------------------------------------------
is_contract_made_test_() ->
    [?_assert(bs_rubber_score:is_contract_made(#contract{level=1}, 7)),
    ?_assert(bs_rubber_score:is_contract_made(#contract{level=2}, 8)),
    ?_assert(bs_rubber_score:is_contract_made(#contract{level=3}, 9)),
    ?_assert(bs_rubber_score:is_contract_made(#contract{level=4}, 10)),
    ?_assert(bs_rubber_score:is_contract_made(#contract{level=5}, 11)),
    ?_assert(bs_rubber_score:is_contract_made(#contract{level=6}, 12)),
    ?_assert(bs_rubber_score:is_contract_made(#contract{level=7}, 13)),
    ?_assertNot(bs_rubber_score:is_contract_made(#contract{level=1}, 6)),
    ?_assertNot(bs_rubber_score:is_contract_made(#contract{level=2}, 7)),
    ?_assertNot(bs_rubber_score:is_contract_made(#contract{level=3}, 8)),
    ?_assertNot(bs_rubber_score:is_contract_made(#contract{level=4}, 9)),
    ?_assertNot(bs_rubber_score:is_contract_made(#contract{level=5}, 10)),
    ?_assertNot(bs_rubber_score:is_contract_made(#contract{level=6}, 11)),
    ?_assertNot(bs_rubber_score:is_contract_made(#contract{level=7}, 12)),
    ?_assertNot(bs_rubber_score:is_contract_made(#contract{level=4}, 8)),
    ?_assertNot(bs_rubber_score:is_contract_made(#contract{level=5}, 8)),
    ?_assertNot(bs_rubber_score:is_contract_made(#contract{level=6}, 9)),
    ?_assertNot(bs_rubber_score:is_contract_made(#contract{level=5}, 10)),
    ?_assert(bs_rubber_score:is_contract_made(#contract{level=2}, 10)),
    ?_assert(bs_rubber_score:is_contract_made(#contract{level=3}, 10)),
    ?_assert(bs_rubber_score:is_contract_made(#contract{level=3}, 11)),
    ?_assert(bs_rubber_score:is_contract_made(#contract{level=4}, 12)),
    ?_assert(bs_rubber_score:is_contract_made(#contract{level=4}, 11)),
    ?_assert(bs_rubber_score:is_contract_made(#contract{level=5}, 13))
    ].


%%--------------------------------------------------------------------------------------------
%% Tests for function 'bs_rubber_score:process_contract_not_made'
%%--------------------------------------------------------------------------------------------
process_contract_not_made_first_entry_test_() ->
    Contract = #contract{owner='NS', color='H', level=5},
    Taken = 8,
    State = #game_state{score=#score{above=[[]], below=[]}, is_NS_vulnerable=false, is_WE_vulnerable=false, round_no=1},
    State1 = State#game_state{is_NS_vulnerable=true},
    State2 = State#game_state{is_WE_vulnerable=true},
    Comment = ?COMMENT__POINTS_FOR_OPPONENTS_UNDERTRICKS,
    [?_assertEqual(State#game_state{score=#score{above=[[#score_entry{count=1, 'WE'=[{150, Comment}], 'NS'=[]}]], below=[]}}, bs_rubber_score:process_contract_not_made(Contract, Taken, State)),
    ?_assertEqual(State1#game_state{score=#score{above=[[#score_entry{count=1, 'NS'=[], 'WE'=[{300, Comment}]}]], below=[]}}, bs_rubber_score:process_contract_not_made(Contract, Taken, State1)),
    ?_assertEqual(State#game_state{score=#score{above=[[#score_entry{count=1, 'WE'=[], 'NS'=[{150, Comment}]}]], below=[]}}, bs_rubber_score:process_contract_not_made(Contract#contract{owner='WE'}, Taken, State)),
    ?_assertEqual(State2#game_state{score=#score{above=[[#score_entry{count=1, 'WE'=[], 'NS'=[{300, Comment}]}]], below=[]}}, bs_rubber_score:process_contract_not_made(Contract#contract{owner='WE'}, Taken, State2))
    ].

process_contract_not_made_test_() ->
    Contract1 = #contract{owner='WE', color='D', level=3},
    Taken1 = 8, % 1 belowtrick
    Contract2 = #contract{owner='NS', color='N', level=3},
    Taken2 = 7, % 2 belowtricks
    Contract5 = #contract{owner='WE', color='S', level=4, doubled=true},
    Taken5 = 8, % 2 belowtricks
    Contract6 = #contract{owner='NS', color='H', level=5, doubled=true},
    Taken6 = 10, % 1 belowtrick
    Contract7 = #contract{owner='NS', color='N', level=2},
    Taken7 = 7, % 1 belowtrick
    Contract8 = #contract{owner='WE', color='C', level=4},
    Taken8 = 9, % 1 belowtrick
    Contract10 = #contract{owner='WE', color='H', level=4},
    Taken10 = 9, % 2 belowtricks
    Contract11 = #contract{owner='NS', color='D', level=5, redoubled=true},
    Taken11 = 9, % 2 belowtricks
    State1 = #game_state{is_NS_vulnerable=false, is_WE_vulnerable=false, round_no=1},
    State2 = State1#game_state{is_NS_vulnerable=true},
    State3 = State2#game_state{is_WE_vulnerable=true},
    Comment = ?COMMENT__POINTS_FOR_OPPONENTS_UNDERTRICKS,
    TE1 = #score_entry{count=1, 'NS'=[{50, Comment}], 'WE'=[]},
    TE2 = #score_entry{count=2, 'NS'=[], 'WE'=[{100, Comment}]},
    TE3 = #score_entry{count=3, 'NS'=[{90, "Scores for contract made"}], 'WE'=[]},
    TE4 = #score_entry{count=4, 'NS'=[{30, "Scores for contract made"}], 'WE'=[]},
    TE5 = #score_entry{count=5, 'NS'=[{300, Comment}], 'WE'=[]},
    TE6 = #score_entry{count=6, 'NS'=[], 'WE'=[{200, Comment}]},
    TE7 = #score_entry{count=7, 'NS'=[], 'WE'=[{100, Comment}]},
    TE8 = #score_entry{count=8, 'NS'=[{50, Comment}], 'WE'=[]},
    TE9 = #score_entry{count=9, 'NS'=[], 'WE'=[{100, "Scores for contract made"}]},
    TE10 = #score_entry{count=10, 'NS'=[{100, Comment}], 'WE'=[]},
    TE11 = #score_entry{count=11, 'NS'=[], 'WE'=[{1000, Comment}]},
    Score0 = #score{above=[], below=[]},
    Score1 = Score0#score{above=[[TE1]]},
    Score2 = Score1#score{above=[[TE1,TE2]]},
    Score3 = Score2#score{below=[TE3]},    
    Score4 = Score3#score{above=[[TE1,TE2],[TE3,TE4]],below=[]},    
    Score5 = Score4#score{above=[[TE1,TE2,TE5],[TE3,TE4]],below=[]},    
    Score6 = Score5#score{above=[[TE1,TE2,TE5,TE6],[TE3,TE4]],below=[]},    
    Score7 = Score6#score{above=[[TE1,TE2,TE5,TE6,TE7],[TE3,TE4]],below=[]},    
    Score8 = Score7#score{above=[[TE1,TE2,TE5,TE6,TE7,TE8],[TE3,TE4]],below=[]},    
    Score9 = Score8#score{above=[[TE1,TE2,TE5,TE6,TE7,TE8],[TE3,TE4],[TE9]],below=[]},    
    Score10 = Score9#score{above=[[TE1,TE2,TE5,TE6,TE7,TE8,TE10],[TE3,TE4],[TE9]],below=[]},    
    Score11 = Score10#score{above=[[TE1,TE2,TE5,TE6,TE7,TE8,TE10,TE11],[TE3,TE4],[TE9]],below=[]},    
    [?_assertEqual(State1#game_state{score=Score1},bs_rubber_score:process_contract_not_made(Contract1, Taken1, State1#game_state{score=Score0, round_no=1})),
    ?_assertEqual(State1#game_state{score=Score2, round_no=2},bs_rubber_score:process_contract_not_made(Contract2, Taken2, State1#game_state{score=Score1, round_no=2})),
    ?_assertEqual(State2#game_state{score=Score5, round_no=5},bs_rubber_score:process_contract_not_made(Contract5, Taken5, State2#game_state{score=Score4, round_no=5})),
    ?_assertEqual(State2#game_state{score=Score6, round_no=6},bs_rubber_score:process_contract_not_made(Contract6, Taken6, State2#game_state{score=Score5, round_no=6})),
    ?_assertEqual(State2#game_state{score=Score7, round_no=7},bs_rubber_score:process_contract_not_made(Contract7, Taken7, State2#game_state{score=Score6, round_no=7})),
    ?_assertEqual(State2#game_state{score=Score8, round_no=8},bs_rubber_score:process_contract_not_made(Contract8, Taken8, State2#game_state{score=Score7, round_no=8})),
    ?_assertEqual(State3#game_state{score=Score10, round_no=10},bs_rubber_score:process_contract_not_made(Contract10, Taken10, State3#game_state{score=Score9, round_no=10})),
    ?_assertEqual(State3#game_state{score=Score11, round_no=11},bs_rubber_score:process_contract_not_made(Contract11, Taken11, State3#game_state{score=Score10, round_no=11}))
    ].
    

%%--------------------------------------------------------------------------------------------------------------------------
%% Tests for counting scores for belowtricks
%%--------------------------------------------------------------------------------------------------------------------------
%% --- Regular ------------------------
count_belowtricks_scores_1before_test() ->
    Contract = #contract{owner='NS', level=3, doubled=false, redoubled=false},
    Taken = 8,
    State = #game_state{is_NS_vulnerable=false},
    Points = bs_rubber_score:count_undertricks_scores(Contract, Taken, State),
    ?_assert(Points =:= 50).
    
count_belowtricks_scores_5before_test() ->
    Contract = #contract{owner='NS', level=5, doubled=false, redoubled=false},
    Taken = 6,
    State = #game_state{is_NS_vulnerable=false},
    Points = bs_rubber_score:count_undertricks_scores(Contract, Taken, State),
    ?_assert(Points =:= 250).
    
count_belowtricks_scores_2after_test() ->
    Contract = #contract{owner='NS', level=4, doubled=false, redoubled=false},
    Taken = 8,
    State = #game_state{is_NS_vulnerable=true},
    Points = bs_rubber_score:count_undertricks_scores(Contract, Taken, State),
    ?_assert(Points =:= 200).

count_belowtricks_scores_6after_test() ->
    Contract = #contract{owner='NS', level=6, doubled=false, redoubled=false},
    Taken = 6,
    State = #game_state{is_NS_vulnerable=true},
    Points = bs_rubber_score:count_undertricks_scores(Contract, Taken, State),
    ?_assert(Points =:= 600).

%% --- Doubled -------------------------
count_belowtricks_scores_3before_dbl_test() ->
    Contract = #contract{owner='NS', level=7, doubled=true, redoubled=false},
    Taken = 10,
    State = #game_state{is_NS_vulnerable=false},
    Points = bs_rubber_score:count_undertricks_scores(Contract, Taken, State),
    ?_assert(Points =:= 500).

count_belowtricks_scores_5before_dbl_test() ->
    Contract = #contract{owner='NS', level=6, doubled=true, redoubled=false},
    Taken = 7,
    State = #game_state{is_NS_vulnerable=false},
    Points = bs_rubber_score:count_undertricks_scores(Contract, Taken, State),
    ?_assert(Points =:= 1100).

count_belowtricks_scores_1after_dbl_test() ->
    Contract = #contract{owner='NS', level=6, doubled=true, redoubled=false},
    Taken = 11,
    State = #game_state{is_NS_vulnerable=true},
    Points = bs_rubber_score:count_undertricks_scores(Contract, Taken, State),
    ?_assert(Points =:= 200).

count_belowtricks_scores_4after_dbl_test() ->
    Contract = #contract{owner='NS', level=5, doubled=true, redoubled=false},
    Taken = 7,
    State = #game_state{is_NS_vulnerable=true},
    Points = bs_rubber_score:count_undertricks_scores(Contract, Taken, State),
    ?_assert(Points =:= 1100).

%% --- Redoubled -----------------------
count_belowtricks_scores_3before_re_test() ->
    Contract = #contract{owner='NS', level=3, doubled=true, redoubled=true},
    Taken = 6,
    State = #game_state{is_NS_vulnerable=false},
    Points = bs_rubber_score:count_undertricks_scores(Contract, Taken, State),
    ?_assert(Points =:= 1000).

count_belowtricks_scores_7before_re_test() ->
    Contract = #contract{owner='NS', level=5, doubled=false, redoubled=true},
    Taken = 4,
    State = #game_state{is_NS_vulnerable=false},
    Points = bs_rubber_score:count_undertricks_scores(Contract, Taken, State),
    ?_assert(Points =:= 3400).

count_belowtricks_scores_2after_re_test() ->
    Contract = #contract{owner='NS', level=3, doubled=true, redoubled=true},
    Taken = 7,
    State = #game_state{is_NS_vulnerable=true},
    Points = bs_rubber_score:count_undertricks_scores(Contract, Taken, State),
    ?_assert(Points =:= 1000).

count_belowtricks_scores_5after_re_test() ->
    Contract = #contract{owner='NS', level=7, doubled=false, redoubled=true},
    Taken = 8,
    State = #game_state{is_NS_vulnerable=true},
    Points = bs_rubber_score:count_undertricks_scores(Contract, Taken, State),
    ?_assert(Points =:= 2800).


%%---------------------------------------------------------------------------------------------------------------------------
%% Tests for function 'bs_rubber_score:create_score_entry_for_contract_not_made'
%%---------------------------------------------------------------------------------------------------------------------------
create_score_entry_for_contract_not_made_test_() ->
    [?_assert(bs_rubber_score:create_score_entry_for_contract_not_made('NS', 200, 1) == #score_entry{count=1, 'NS'=[], 'WE'=[{200, ?COMMENT__POINTS_FOR_OPPONENTS_UNDERTRICKS}]}),
    ?_assert(bs_rubber_score:create_score_entry_for_contract_not_made('NS', 100, 3) == #score_entry{count=3, 'NS'=[], 'WE'=[{100, ?COMMENT__POINTS_FOR_OPPONENTS_UNDERTRICKS}]}),
    ?_assert(bs_rubber_score:create_score_entry_for_contract_not_made('WE', 50, 4) == #score_entry{count=4, 'WE'=[], 'NS'=[{50, ?COMMENT__POINTS_FOR_OPPONENTS_UNDERTRICKS}]}),
    ?_assert(bs_rubber_score:create_score_entry_for_contract_not_made('WE', 150, 8) == #score_entry{count=8, 'WE'=[], 'NS'=[{150, ?COMMENT__POINTS_FOR_OPPONENTS_UNDERTRICKS}]}),
    ?_assertException(error, function_clause, bs_rubber_score:create_score_entry_for_contract_not_made(any, 100, 1))
    ].


%%---------------------------------------------------------------------------------------------------------------------------
%% Tests for function 'bs_rubber_score:create_score_entry_for_contract_made'
%%---------------------------------------------------------------------------------------------------------------------------
create_score_entry_for_contract_made_test_() ->
    [?_assert(bs_rubber_score:create_score_entry_for_contract_made('NS', 200, 1) == #score_entry{count=1, 'WE'=[], 'NS'=[{200, ?COMMENT__POINTS_FOR_CONTRACT_MADE}]}),
    ?_assert(bs_rubber_score:create_score_entry_for_contract_made('NS', 100, 3) == #score_entry{count=3, 'WE'=[], 'NS'=[{100, ?COMMENT__POINTS_FOR_CONTRACT_MADE}]}),
    ?_assert(bs_rubber_score:create_score_entry_for_contract_made('WE', 50, 4) == #score_entry{count=4, 'NS'=[], 'WE'=[{50, ?COMMENT__POINTS_FOR_CONTRACT_MADE}]}),
    ?_assert(bs_rubber_score:create_score_entry_for_contract_made('WE', 150, 8) == #score_entry{count=8, 'NS'=[], 'WE'=[{150, ?COMMENT__POINTS_FOR_CONTRACT_MADE}]}),
    ?_assertException(error, function_clause, bs_rubber_score:create_score_entry_for_contract_made(any, 100, 1))
    ].


%%---------------------------------------------------------------------------------------------------------------------------
%% Tests for function 'insert_score_entry'
%%---------------------------------------------------------------------------------------------------------------------------
insert_score_entry_test_() ->
    TE1 = #score_entry{count=1, 'NS'=[{100, "Test"}], 'WE'=[{50, "Test"}]},
    TE2 = TE1#score_entry{count=2},
    TE3 = TE1#score_entry{count=3},
    TE4 = TE1#score_entry{count=4},
    TE5 = TE1#score_entry{count=5},
    TE6 = TE1#score_entry{count=6},
    ETE = #score_entry{count=7, 'NS'=[], 'WE'=[]},
    [?_assertEqual(#score{below=[TE1]}, bs_rubber_score:insert_score_entry(below, #score{below=[]}, TE1)),
    ?_assertEqual(#score{below=[TE1,TE2,TE3,TE4]} , bs_rubber_score:insert_score_entry(below, #score{below=[TE1,TE2,TE3]}, TE4)), 
    ?_assertEqual(#score{above=[[TE1,TE2,TE3,TE4],[TE6]]}, bs_rubber_score:insert_score_entry(above, #score{above=[[TE1,TE2,TE3],[TE6]]}, TE4)), 
    ?_assertEqual(#score{above=[[TE1,TE2,TE3,TE4]]},  bs_rubber_score:insert_score_entry(above, #score{above=[[TE1,TE2,TE3]]}, TE4)), 
    ?_assertEqual(#score{above=[[TE1,TE2,TE4],[TE3]]}, bs_rubber_score:insert_score_entry(above, #score{above=[[TE1,TE2],[TE3]]}, TE4)), 
    ?_assertEqual(#score{above=[[TE1,TE2,TE5],[TE3]], below=[TE4]}, bs_rubber_score:insert_score_entry(above, #score{above=[[TE1,TE2],[TE3]], below=[TE4]}, TE5)),
    ?_assertException(error, function_clause, bs_rubber_score:insert_score_entry(any, #score{}, TE1)),
    ?_assertEqual(#score{below=[]}, bs_rubber_score:insert_score_entry(below, #score{below=[]}, ETE)),
    ?_assertEqual(#score{above=[]}, bs_rubber_score:insert_score_entry(above, #score{above=[]}, ETE)),
    ?_assertEqual(#score{above=[[TE1,TE2,TE3]]},  bs_rubber_score:insert_score_entry(above, #score{above=[[TE1,TE2,TE3]]}, ETE)), 
    ?_assertEqual(#score{below=[[TE1,TE2,TE3]]},  bs_rubber_score:insert_score_entry(below, #score{below=[[TE1,TE2,TE3]]}, ETE)), 
    ?_assertEqual(#score{above=[[TE1,TE2],[TE3]], below=[TE4]}, bs_rubber_score:insert_score_entry(above, #score{above=[[TE1,TE2],[TE3]], below=[TE4]}, ETE)),
    ?_assertEqual(#score{above=[[TE1,TE2],[TE3]], below=[TE4]}, bs_rubber_score:insert_score_entry(below, #score{above=[[TE1,TE2],[TE3]], below=[TE4]}, ETE))
    ].


%%---------------------------------------------------------------------------------------------------------------------------
%% Tests for bs_rubber_score:processing succesful contract 
%%---------------------------------------------------------------------------------------------------------------------------
process_contract_made_test_() ->
    Contract1 = #contract{owner='WE', color='D', level=3},
    Taken1 = 11, % 2 overtricks
    Contract3 = #contract{owner='NS', color='H', level=6},
    Taken3 = 13, % small slam
    Contract4 = #contract{owner='NS', color='S', level=1},
    Taken4 = 7,
    Contract5 = #contract{owner='WE', color='S', level=4, doubled=true},
    Taken5 = 10, % 
    Contract7 = #contract{owner='WE', color='N', level=2},
    Taken7 = 8, % 
    Contract8 = #contract{owner='WE', color='C', level=2, doubled=true},
    Taken8 = 9, % 1 overtrick
    State1 = #game_state{is_NS_vulnerable=false, is_WE_vulnerable=false, round_no=1},
    State2 = #game_state{is_NS_vulnerable=true, is_WE_vulnerable=false},
    State3 = #game_state{is_NS_vulnerable=true, is_WE_vulnerable=true},
    TE1A = #score_entry{count=1, 'WE'=[{40, ?COMMENT__OVERTRICKS_POINTS}], 'NS'=[]},
    TE1U = #score_entry{count=1, 'WE'=[{60, ?COMMENT__POINTS_FOR_CONTRACT_MADE}], 'NS'=[]},
    TE3A = #score_entry{count=3, 'NS'=[{30, ?COMMENT__OVERTRICKS_POINTS}, {500, ?COMMENT__SMALL_SLAM_BONUS}], 'WE'=[]},
    TE3U = #score_entry{count=3, 'NS'=[{180, ?COMMENT__POINTS_FOR_CONTRACT_MADE}], 'WE'=[]},
    TE4 = #score_entry{count=4, 'NS'=[{30, ?COMMENT__POINTS_FOR_CONTRACT_MADE}], 'WE'=[]},
    TE5A = #score_entry{count=5, 'NS'=[], 'WE'=[{50, ?COMMENT__WON_DOUBLED_GAME}]},
    TE5U = #score_entry{count=5, 'NS'=[], 'WE'=[{240, ?COMMENT__POINTS_FOR_CONTRACT_MADE}]},
    TE7 = #score_entry{count=7, 'NS'=[], 'WE'=[{70, ?COMMENT__POINTS_FOR_CONTRACT_MADE}]},
    TE8A = #score_entry{count=8, 'NS'=[], 'WE'=[{200, ?COMMENT__OVERTRICKS_POINTS}, {50, ?COMMENT__WON_DOUBLED_GAME}, {500, ?COMMENT__WINNING_RUBBER_BONUS}]},
    TE8U = #score_entry{count=8, 'NS'=[], 'WE'=[{80, ?COMMENT__POINTS_FOR_CONTRACT_MADE}]},
    Score0 = #score{above=[], below=[]},
    Score1 = Score0#score{above=[[TE1A]], below=[TE1U]},
    Score3 = Score1#score{above=[[TE1A, TE3A], [TE1U, TE3U]], below=[]},    
    Score4 = Score3#score{below=[TE4]},    
    Score5 = Score4#score{above=[[TE1A, TE3A, TE5A], [TE1U, TE3U], [TE4, TE5U]],below=[]},    
    Score7 = Score5#score{above=[[TE1A, TE3A, TE5A], [TE1U, TE3U], [TE4, TE5U]], below=[TE7]},    
    Score8 = Score7#score{above=[[TE1A, TE3A, TE5A, TE8A], [TE1U, TE3U], [TE4, TE5U], [TE7, TE8U]], below=[]},    
    Summary = #summary{winner='WE', ns_score=740, we_score=1290},
    [
    ?_assertEqual(State1#game_state{score=Score1, round_no=1}, bs_rubber_score:process_contract_made(Contract1, Taken1, State1#game_state{score=Score0, round_no=1}, [])),
    ?_assertEqual(State2#game_state{score=Score3, round_no=3}, bs_rubber_score:process_contract_made(Contract3, Taken3, State1#game_state{score=Score1, round_no=3}, [])),
    ?_assertEqual(State2#game_state{score=Score4, round_no=4}, bs_rubber_score:process_contract_made(Contract4, Taken4, State2#game_state{score=Score3, round_no=4}, [])),
    ?_assertEqual(State3#game_state{score=Score5, round_no=5}, bs_rubber_score:process_contract_made(Contract5, Taken5, State2#game_state{score=Score4, round_no=5}, [])),
    ?_assertEqual(State3#game_state{score=Score7, round_no=7}, bs_rubber_score:process_contract_made(Contract7, Taken7, State3#game_state{score=Score5, round_no=7}, [])),
    ?_assertEqual(State3#game_state{score=Score8, round_no=8, status=Summary}, bs_rubber_score:process_contract_made(Contract8, Taken8, State3#game_state{score=Score7, round_no=8}, []))
    ].


%%---------------------------------------------------------------------------------------------------------------------------
%% Tests for counting scores for contract made
%%---------------------------------------------------------------------------------------------------------------------------
count_score_for_contract_test_() ->
    [?_assertEqual(120, bs_rubber_score:count_score_for_contract(#contract{color='C', level=6, doubled=false})),
    ?_assertEqual(120, bs_rubber_score:count_score_for_contract(#contract{color='C', level=3, doubled=true})),
    ?_assertEqual(100, bs_rubber_score:count_score_for_contract(#contract{color='C', level=5})),
    ?_assertEqual(40, bs_rubber_score:count_score_for_contract(#contract{color='C', level=2})),
    ?_assertEqual(160, bs_rubber_score:count_score_for_contract(#contract{color='C', level=2, redoubled=true})),
    ?_assertEqual(80, bs_rubber_score:count_score_for_contract(#contract{color='D', level=4})),
    ?_assertEqual(160, bs_rubber_score:count_score_for_contract(#contract{color='D', level=4, doubled=true})),
    ?_assertEqual(240, bs_rubber_score:count_score_for_contract(#contract{color='D', level=3, redoubled=true})),
    ?_assertEqual(60, bs_rubber_score:count_score_for_contract(#contract{color='H', level=2})),
    ?_assertEqual(30, bs_rubber_score:count_score_for_contract(#contract{color='H', level=1})),
    ?_assertEqual(60, bs_rubber_score:count_score_for_contract(#contract{color='H', level=1, doubled=true})),
    ?_assertEqual(120, bs_rubber_score:count_score_for_contract(#contract{color='H', level=1, redoubled=true})),
    ?_assertEqual(150, bs_rubber_score:count_score_for_contract(#contract{color='H', level=5})),
    ?_assertEqual(150, bs_rubber_score:count_score_for_contract(#contract{color='S', level=5})),
    ?_assertEqual(300, bs_rubber_score:count_score_for_contract(#contract{color='S', level=5, doubled=true})),
    ?_assertEqual(180, bs_rubber_score:count_score_for_contract(#contract{color='S', level=3, doubled=true})),
    ?_assertEqual(240, bs_rubber_score:count_score_for_contract(#contract{color='S', level=2, redoubled=true})),
    ?_assertEqual(40, bs_rubber_score:count_score_for_contract(#contract{color='N', level=1})),
    ?_assertEqual(70, bs_rubber_score:count_score_for_contract(#contract{color='N', level=2})),
    ?_assertEqual(100, bs_rubber_score:count_score_for_contract(#contract{color='N', level=3})),
    ?_assertEqual(200, bs_rubber_score:count_score_for_contract(#contract{color='N', level=3, doubled=true})),
    ?_assertEqual(140, bs_rubber_score:count_score_for_contract(#contract{color='N', level=2, doubled=true})),
    ?_assertEqual(640, bs_rubber_score:count_score_for_contract(#contract{color='N', level=5, doubled=true, redoubled=true}))
    ].


%%---------------------------------------------------------------------------------------------------------------------------
%% Tests for counting bonus points
%%---------------------------------------------------------------------------------------------------------------------------
create_score_entry_for_bonuses_overtricks_and_double_done_test_() ->
    State1 = #game_state{round_no=1, is_NS_vulnerable=false, is_WE_vulnerable=false},
    State2 = #game_state{round_no=1, is_NS_vulnerable=true, is_WE_vulnerable=false},
    Contract1 = #contract{owner='NS', color='C', level=2},
    Taken1 = 8,
    SE1 = #score_entry{count=1, 'NS'=[], 'WE'=[]},
    Contract2 = Contract1,
    Taken2 = 10,
    SE2 = #score_entry{count=1, 'NS'=[{40, ?COMMENT__OVERTRICKS_POINTS}], 'WE'=[]},
    Contract3 = #contract{color='H', owner='NS', level=2},
    Taken3 = 9,
    SE3 = #score_entry{count=1, 'NS'=[{30, ?COMMENT__OVERTRICKS_POINTS}], 'WE'=[]},
    Contract4 = #contract{color='H', owner='NS', level=2, doubled=true},
    Taken4 = 9,
    SE4 = #score_entry{count=1, 'NS'=[{100, ?COMMENT__OVERTRICKS_POINTS}, {50, ?COMMENT__WON_DOUBLED_GAME}], 'WE'=[]},
    SE42 = #score_entry{count=1, 'NS'=[{200, ?COMMENT__OVERTRICKS_POINTS}, {50, ?COMMENT__WON_DOUBLED_GAME}], 'WE'=[]},
    Contract5 = #contract{color='H', owner='NS', level=2, redoubled=true},
    Taken5 = 11,
    SE5 = #score_entry{count=1, 'NS'=[{600, ?COMMENT__OVERTRICKS_POINTS}, {50, ?COMMENT__WON_DOUBLED_GAME}], 'WE'=[]},
    SE52 = #score_entry{count=1, 'NS'=[{1200, ?COMMENT__OVERTRICKS_POINTS}, {50, ?COMMENT__WON_DOUBLED_GAME}], 'WE'=[]},
    Contract6 = #contract{owner='NS', color='N', level=3},
    Taken6 = 11,
    SE6 = #score_entry{count=1, 'NS'=[{70, ?COMMENT__OVERTRICKS_POINTS}], 'WE'=[]},
    Contract7 = #contract{owner='NS', color='N', level=3, doubled=true},
    Taken7 = 11,
    SE7 = #score_entry{count=1, 'NS'=[{200, ?COMMENT__OVERTRICKS_POINTS}, {50, ?COMMENT__WON_DOUBLED_GAME}], 'WE'=[]},
    SE72 = #score_entry{count=1, 'NS'=[{400, ?COMMENT__OVERTRICKS_POINTS}, {50, ?COMMENT__WON_DOUBLED_GAME}], 'WE'=[]},
    [?_assertEqual(SE1, bs_rubber_score:create_score_entry_for_bonuses(Contract1, Taken1, State1, [], #score{})),
    ?_assertEqual(SE2, bs_rubber_score:create_score_entry_for_bonuses(Contract2, Taken2, State1, [], #score{})),
    ?_assertEqual(SE3, bs_rubber_score:create_score_entry_for_bonuses(Contract3, Taken3, State1, [], #score{})),
    ?_assertEqual(SE4, bs_rubber_score:create_score_entry_for_bonuses(Contract4, Taken4, State1, [], #score{})),
    ?_assertEqual(SE5, bs_rubber_score:create_score_entry_for_bonuses(Contract5, Taken5, State1, [], #score{})),
    ?_assertEqual(SE6, bs_rubber_score:create_score_entry_for_bonuses(Contract6, Taken6, State1, [], #score{})),
    ?_assertEqual(SE7, bs_rubber_score:create_score_entry_for_bonuses(Contract7, Taken7, State1, [], #score{})),
    ?_assertEqual(SE1, bs_rubber_score:create_score_entry_for_bonuses(Contract1, Taken1, State2, [], #score{})),
    ?_assertEqual(SE2, bs_rubber_score:create_score_entry_for_bonuses(Contract2, Taken2, State2, [], #score{})),
    ?_assertEqual(SE3, bs_rubber_score:create_score_entry_for_bonuses(Contract3, Taken3, State2, [], #score{})),
    ?_assertEqual(SE42, bs_rubber_score:create_score_entry_for_bonuses(Contract4, Taken4, State2, [], #score{})),
    ?_assertEqual(SE52, bs_rubber_score:create_score_entry_for_bonuses(Contract5, Taken5, State2, [], #score{})),
    ?_assertEqual(SE6, bs_rubber_score:create_score_entry_for_bonuses(Contract6, Taken6, State2, [], #score{})),
    ?_assertEqual(SE72, bs_rubber_score:create_score_entry_for_bonuses(Contract7, Taken7, State2, [], #score{}))
    ].

create_score_entry_for_bonuses_slam_test_() ->
    State1 = #game_state{round_no=1, is_NS_vulnerable=false, is_WE_vulnerable=false},
    State2 = #game_state{round_no=1, is_NS_vulnerable=true, is_WE_vulnerable=false},
    State3 = #game_state{round_no=1, is_NS_vulnerable=false, is_WE_vulnerable=true},
    Contract1 = #contract{owner='NS', color='C', level=6},
    Contract2 = #contract{owner='NS', color='D', level=7},
    Contract3 = #contract{owner='NS', color='H', level=6, doubled=true},
    Contract4 = #contract{owner='NS', color='S', level=7, doubled=true},
    Contract5 = #contract{owner='NS', color='N', level=6, redoubled=true},
    Contract6 = #contract{owner='NS', color='C', level=7, redoubled=true},
    Contract7 = #contract{owner='WE', color='S', level=6},
    Contract8 = #contract{owner='WE', color='H', level=7},
    Contract9 = #contract{owner='WE', color='S', level=6, doubled=true},
    Contract10 = #contract{owner='WE', color='H', level=7, doubled=true},
    Contract11 = #contract{owner='WE', color='N', level=6, redoubled=true},
    Contract12 = #contract{owner='WE', color='C', level=7, redoubled=true},
    Score1 = {500, ?COMMENT__SMALL_SLAM_BONUS},
    Score2 = {750, ?COMMENT__SMALL_SLAM_BONUS},
    Score3 = {1000, ?COMMENT__GRAND_SLAM_BONUS},
    Score4 = {1500, ?COMMENT__GRAND_SLAM_BONUS},
    Score5 = {50, ?COMMENT__WON_DOUBLED_GAME},
    [?_assertEqual(#score_entry{count=1, 'NS'=[Score1], 'WE'=[]}, bs_rubber_score:create_score_entry_for_bonuses(Contract1, 12, State1, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'NS'=[{20, ?COMMENT__OVERTRICKS_POINTS}, Score1], 'WE'=[]}, bs_rubber_score:create_score_entry_for_bonuses(Contract1, 13, State1, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'NS'=[Score3], 'WE'=[]}, bs_rubber_score:create_score_entry_for_bonuses(Contract2, 13, State1, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'NS'=[Score2], 'WE'=[]}, bs_rubber_score:create_score_entry_for_bonuses(Contract1, 12, State2, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'NS'=[{20, ?COMMENT__OVERTRICKS_POINTS}, Score2], 'WE'=[]}, bs_rubber_score:create_score_entry_for_bonuses(Contract1, 13, State2, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'NS'=[Score4], 'WE'=[]}, bs_rubber_score:create_score_entry_for_bonuses(Contract2, 13, State2, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'NS'=[Score1, Score5], 'WE'=[]}, bs_rubber_score:create_score_entry_for_bonuses(Contract3, 12, State1, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'NS'=[{100, ?COMMENT__OVERTRICKS_POINTS}, Score1, Score5], 'WE'=[]}, bs_rubber_score:create_score_entry_for_bonuses(Contract3, 13, State1, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'NS'=[Score3, Score5], 'WE'=[]}, bs_rubber_score:create_score_entry_for_bonuses(Contract4, 13, State1, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'NS'=[Score2, Score5], 'WE'=[]}, bs_rubber_score:create_score_entry_for_bonuses(Contract3, 12, State2, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'NS'=[{200, ?COMMENT__OVERTRICKS_POINTS}, Score2, Score5], 'WE'=[]}, bs_rubber_score:create_score_entry_for_bonuses(Contract3, 13, State2, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'NS'=[Score4, Score5], 'WE'=[]}, bs_rubber_score:create_score_entry_for_bonuses(Contract4, 13, State2, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'NS'=[Score1, Score5], 'WE'=[]}, bs_rubber_score:create_score_entry_for_bonuses(Contract5, 12, State1, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'NS'=[{200, ?COMMENT__OVERTRICKS_POINTS}, Score1, Score5], 'WE'=[]}, bs_rubber_score:create_score_entry_for_bonuses(Contract5, 13, State1, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'NS'=[Score3, Score5], 'WE'=[]}, bs_rubber_score:create_score_entry_for_bonuses(Contract6, 13, State1, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'NS'=[Score2, Score5], 'WE'=[]}, bs_rubber_score:create_score_entry_for_bonuses(Contract5, 12, State2, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'NS'=[{400, ?COMMENT__OVERTRICKS_POINTS}, Score2, Score5], 'WE'=[]}, bs_rubber_score:create_score_entry_for_bonuses(Contract5, 13, State2, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'NS'=[Score4, Score5], 'WE'=[]}, bs_rubber_score:create_score_entry_for_bonuses(Contract6, 13, State2, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'WE'=[Score1], 'NS'=[]}, bs_rubber_score:create_score_entry_for_bonuses(Contract7, 12, State1, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'WE'=[{30, ?COMMENT__OVERTRICKS_POINTS}, Score1], 'NS'=[]}, bs_rubber_score:create_score_entry_for_bonuses(Contract7, 13, State1, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'WE'=[Score3], 'NS'=[]}, bs_rubber_score:create_score_entry_for_bonuses(Contract8, 13, State1, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'WE'=[Score2], 'NS'=[]}, bs_rubber_score:create_score_entry_for_bonuses(Contract7, 12, State3, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'WE'=[{30, ?COMMENT__OVERTRICKS_POINTS}, Score2], 'NS'=[]}, bs_rubber_score:create_score_entry_for_bonuses(Contract7, 13, State3, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'WE'=[Score4], 'NS'=[]}, bs_rubber_score:create_score_entry_for_bonuses(Contract8, 13, State3, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'WE'=[Score1, Score5], 'NS'=[]}, bs_rubber_score:create_score_entry_for_bonuses(Contract9, 12, State1, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'WE'=[{100, ?COMMENT__OVERTRICKS_POINTS}, Score1, Score5], 'NS'=[]}, bs_rubber_score:create_score_entry_for_bonuses(Contract9, 13, State1, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'WE'=[Score3, Score5], 'NS'=[]}, bs_rubber_score:create_score_entry_for_bonuses(Contract10, 13, State1, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'WE'=[Score2, Score5], 'NS'=[]}, bs_rubber_score:create_score_entry_for_bonuses(Contract9, 12, State3, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'WE'=[{200, ?COMMENT__OVERTRICKS_POINTS}, Score2, Score5], 'NS'=[]}, bs_rubber_score:create_score_entry_for_bonuses(Contract9, 13, State3, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'WE'=[Score4, Score5], 'NS'=[]}, bs_rubber_score:create_score_entry_for_bonuses(Contract10, 13, State3, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'WE'=[Score1, Score5], 'NS'=[]}, bs_rubber_score:create_score_entry_for_bonuses(Contract11, 12, State1, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'WE'=[{200, ?COMMENT__OVERTRICKS_POINTS}, Score1, Score5], 'NS'=[]}, bs_rubber_score:create_score_entry_for_bonuses(Contract11, 13, State1, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'WE'=[Score3, Score5], 'NS'=[]}, bs_rubber_score:create_score_entry_for_bonuses(Contract12, 13, State1, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'WE'=[Score2, Score5], 'NS'=[]}, bs_rubber_score:create_score_entry_for_bonuses(Contract11, 12, State3, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'WE'=[{400, ?COMMENT__OVERTRICKS_POINTS}, Score2, Score5], 'NS'=[]}, bs_rubber_score:create_score_entry_for_bonuses(Contract11, 13, State3, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'WE'=[Score4, Score5], 'NS'=[]}, bs_rubber_score:create_score_entry_for_bonuses(Contract12, 13, State3, [], #score{}))
    ].
    
create_score_entry_for_bonuses_rubber_won_test_() ->
    [].

create_score_entry_for_bonuses_extra_points_test_() ->
    [].



%%---------------------------------------------------------------------------------------------------------------------------
%% Test for function checking that rubber is finished
%%---------------------------------------------------------------------------------------------------------------------------
is_game_won_test_() ->
    TE1 = #score_entry{'NS'=[{30, ?COMMENT__POINTS_FOR_CONTRACT_MADE}], 'WE'=[]},
    TE2 = #score_entry{'NS'=[{60, ?COMMENT__POINTS_FOR_CONTRACT_MADE}], 'WE'=[]},
    TE3 = #score_entry{'NS'=[{90, ?COMMENT__POINTS_FOR_CONTRACT_MADE}], 'WE'=[]},
    TE4 = #score_entry{'WE'=[{30, ?COMMENT__POINTS_FOR_CONTRACT_MADE}], 'NS'=[]},
    TE5 = #score_entry{'WE'=[{60, ?COMMENT__POINTS_FOR_CONTRACT_MADE}], 'NS'=[]},
    TE6 = #score_entry{'WE'=[{90, ?COMMENT__POINTS_FOR_CONTRACT_MADE}], 'NS'=[]},
    TE7 = #score_entry{'NS'=[{120, ?COMMENT__POINTS_FOR_CONTRACT_MADE}], 'WE'=[]},
    TE8 = #score_entry{'NS'=[{150, ?COMMENT__POINTS_FOR_CONTRACT_MADE}], 'WE'=[]},
    TE9 = #score_entry{'NS'=[{300, ?COMMENT__POINTS_FOR_CONTRACT_MADE}], 'WE'=[]},
    TE10 = #score_entry{'NS'=[{100, ?COMMENT__POINTS_FOR_CONTRACT_MADE}], 'WE'=[]},
    TE11 = #score_entry{'WE'=[{120, ?COMMENT__POINTS_FOR_CONTRACT_MADE}], 'NS'=[]},
    TE12 = #score_entry{'WE'=[{150, ?COMMENT__POINTS_FOR_CONTRACT_MADE}], 'NS'=[]},
    TE13 = #score_entry{'WE'=[{300, ?COMMENT__POINTS_FOR_CONTRACT_MADE}], 'NS'=[]},
    TE14 = #score_entry{'WE'=[{100, ?COMMENT__POINTS_FOR_CONTRACT_MADE}], 'NS'=[]},
    [?_assertNot(bs_rubber_score:is_game_won(#score{below=[]})), 
    ?_assertNot(bs_rubber_score:is_game_won(#score{below=[TE1]})),
    ?_assertNot(bs_rubber_score:is_game_won(#score{below=[TE2]})),
    ?_assertNot(bs_rubber_score:is_game_won(#score{below=[TE3]})),
    ?_assertNot(bs_rubber_score:is_game_won(#score{below=[TE1, TE1]})),
    ?_assertNot(bs_rubber_score:is_game_won(#score{below=[TE1, TE2]})),
    ?_assertNot(bs_rubber_score:is_game_won(#score{below=[TE1, TE1, TE1]})),
    ?_assertNot(bs_rubber_score:is_game_won(#score{below=[TE4]})),
    ?_assertNot(bs_rubber_score:is_game_won(#score{below=[TE5]})),
    ?_assertNot(bs_rubber_score:is_game_won(#score{below=[TE6]})),
    ?_assertNot(bs_rubber_score:is_game_won(#score{below=[TE4, TE4]})),
    ?_assertNot(bs_rubber_score:is_game_won(#score{below=[TE4, TE5]})),
    ?_assertNot(bs_rubber_score:is_game_won(#score{below=[TE4, TE4, TE4]})),
    ?_assertNot(bs_rubber_score:is_game_won(#score{below=[TE1, TE5]})),
    ?_assertNot(bs_rubber_score:is_game_won(#score{below=[TE2, TE4]})),
    ?_assertNot(bs_rubber_score:is_game_won(#score{below=[TE3, TE6]})),
    ?_assertNot(bs_rubber_score:is_game_won(#score{below=[TE1, TE1, TE5, TE4]})),
    ?_assertNot(bs_rubber_score:is_game_won(#score{below=[TE1, TE2, TE4, TE4]})),
    ?_assertNot(bs_rubber_score:is_game_won(#score{below=[TE1, TE1, TE1, TE6]})),
    ?_assert(bs_rubber_score:is_game_won(#score{below=[TE7]})),
    ?_assert(bs_rubber_score:is_game_won(#score{below=[TE8]})),
    ?_assert(bs_rubber_score:is_game_won(#score{below=[TE9]})),
    ?_assert(bs_rubber_score:is_game_won(#score{below=[TE10]})),
    ?_assert(bs_rubber_score:is_game_won(#score{below=[TE11]})),
    ?_assert(bs_rubber_score:is_game_won(#score{below=[TE12]})),
    ?_assert(bs_rubber_score:is_game_won(#score{below=[TE13]})),
    ?_assert(bs_rubber_score:is_game_won(#score{below=[TE14]})),
    ?_assert(bs_rubber_score:is_game_won(#score{below=[TE5, TE4, TE10]})),
    ?_assert(bs_rubber_score:is_game_won(#score{below=[TE6, TE4]})),
    ?_assert(bs_rubber_score:is_game_won(#score{below=[TE6, TE3, TE4]})),
    ?_assert(bs_rubber_score:is_game_won(#score{below=[TE3, TE6, TE8]}))
    ].


%%---------------------------------------------------------------------------------------------------------------------------
%% Tests for closing game
%%---------------------------------------------------------------------------------------------------------------------------
close_game_test_() ->
    TE1A = #score_entry{count=1, 'WE'=[{40, ?COMMENT__OVERTRICKS_POINTS}], 'NS'=[]},
    TE1U = #score_entry{count=1, 'WE'=[{160, ?COMMENT__POINTS_FOR_CONTRACT_MADE}], 'NS'=[]},
    TE2A = #score_entry{count=3, 'NS'=[{30, ?COMMENT__OVERTRICKS_POINTS}, {500, ?COMMENT__SMALL_SLAM_BONUS}], 'WE'=[]},
    TE2U = #score_entry{count=3, 'NS'=[{180, ?COMMENT__POINTS_FOR_CONTRACT_MADE}], 'WE'=[]},
    TE6 = #score_entry{count=6, 'NS'=[], 'WE'=[{400, ?COMMENT__POINTS_FOR_OPPONENTS_UNDERTRICKS}]},
    Score1 = #score{above=[[TE1A]], below=[TE1U]},
    ExpScore1 = #score{above=[[TE1A],[TE1U]], below=[]},
    State1 = #game_state{score=Score1, is_NS_vulnerable=false, is_WE_vulnerable=false},
    ExpState1 = #game_state{score=ExpScore1, is_NS_vulnerable=false, is_WE_vulnerable=true, status=unfinished},
    Score2 = #score{above=[[TE2A]], below=[TE2U]},
    ExpScore2 = #score{above=[[TE2A],[TE2U]], below=[]},
    State2 = #game_state{score=Score2, is_NS_vulnerable=false, is_WE_vulnerable=false},
    ExpState2 = #game_state{score=ExpScore2, is_NS_vulnerable=true, is_WE_vulnerable=false, status=unfinished},
    Score3 = #score{above=[[TE6, TE1A]], below=[TE1U]},
    ExpScore3 = #score{above=[[TE6, TE1A],[TE1U]], below=[]},
    State3 = #game_state{score=Score3, is_NS_vulnerable=true, is_WE_vulnerable=false},
    ExpState3 = #game_state{score=ExpScore3, is_NS_vulnerable=true, is_WE_vulnerable=true, status=unfinished},
    Score4 = #score{above=[[TE6, TE2A]], below=[TE2U]},
    ExpScore4 = #score{above=[[TE6, TE2A],[TE2U]], below=[]},
    State4 = #game_state{score=Score4, is_NS_vulnerable=false, is_WE_vulnerable=true},
    ExpState4 = #game_state{score=ExpScore4, is_NS_vulnerable=true, is_WE_vulnerable=true, status=unfinished},
    Score5 = #score{above=[[TE6, TE1A]], below=[TE1U]},
    ExpScore5 = #score{above=[[TE6, TE1A],[TE1U]], below=[]},
    State5 = #game_state{score=Score5, is_NS_vulnerable=true, is_WE_vulnerable=true},
    ExpState5 = #game_state{score=ExpScore5, is_NS_vulnerable=true, is_WE_vulnerable=true, status=#summary{winner='WE', we_score=600, ns_score=0}},
    Score6 = #score{above=[[TE6, TE2A]], below=[TE2U]},
    ExpScore6 = #score{above=[[TE6, TE2A],[TE2U]], below=[]},
    State6 = #game_state{score=Score6, is_NS_vulnerable=true, is_WE_vulnerable=true},
    ExpState6 = #game_state{score=ExpScore6, is_NS_vulnerable=true, is_WE_vulnerable=true, status=#summary{winner='NS', we_score=400, ns_score=710}},
    Score7 = #score{above=[[]], below=[TE1U]},
    ExpScore7 = #score{above=[[],[TE1U]], below=[]},
    State7 = #game_state{score=Score7, is_NS_vulnerable=false, is_WE_vulnerable=false},
    ExpState7 = #game_state{score=ExpScore7, is_NS_vulnerable=false, is_WE_vulnerable=true, status=unfinished},
    [?_assertEqual(ExpState1, bs_rubber_score:close_game(State1)),
    ?_assertEqual(ExpState2, bs_rubber_score:close_game(State2)),
    ?_assertEqual(ExpState3, bs_rubber_score:close_game(State3)),
    ?_assertEqual(ExpState4, bs_rubber_score:close_game(State4)),
    ?_assertEqual(ExpState5, bs_rubber_score:close_game(State5)),
    ?_assertEqual(ExpState6, bs_rubber_score:close_game(State6)),
    ?_assertEqual(ExpState7, bs_rubber_score:close_game(State7))
    ].


%%---------------------------------------------------------------------------------------------------------------------------
%% Test for moving scores from below to above
%%---------------------------------------------------------------------------------------------------------------------------
move_points_from_below_to_above_test_() ->
    [?_assertEqual(#score{above=[[bonuses],[points_from_below]], below=[]}, bs_rubber_score:move_points_from_below_to_above(#score{above=[[bonuses]], below=[points_from_below]})),
    ?_assertEqual(#score{above=[[bonuses],[points_from_first_game],[points_from_last_game]], below=[]}, bs_rubber_score:move_points_from_below_to_above(#score{above=[[bonuses],[points_from_first_game]], below=[points_from_last_game]})),
    ?_assertEqual(#score{above=[[],[points_from_below]], below=[]}, bs_rubber_score:move_points_from_below_to_above(#score{above=[[]], below=[points_from_below]}))
    ].


%%---------------------------------------------------------------------------------------------------------------------------
%% Test for function checking who won the game
%%---------------------------------------------------------------------------------------------------------------------------
who_won_game_test_() ->
    TE3 = #score_entry{'NS'=[{90, ?COMMENT__POINTS_FOR_CONTRACT_MADE}], 'WE'=[]},
    TE4 = #score_entry{'WE'=[{30, ?COMMENT__POINTS_FOR_CONTRACT_MADE}], 'NS'=[]},
    TE5 = #score_entry{'WE'=[{60, ?COMMENT__POINTS_FOR_CONTRACT_MADE}], 'NS'=[]},
    TE6 = #score_entry{'WE'=[{90, ?COMMENT__POINTS_FOR_CONTRACT_MADE}], 'NS'=[]},
    TE7 = #score_entry{'NS'=[{120, ?COMMENT__POINTS_FOR_CONTRACT_MADE}], 'WE'=[]},
    TE8 = #score_entry{'NS'=[{150, ?COMMENT__POINTS_FOR_CONTRACT_MADE}], 'WE'=[]},
    TE9 = #score_entry{'NS'=[{300, ?COMMENT__POINTS_FOR_CONTRACT_MADE}], 'WE'=[]},
    TE10 = #score_entry{'NS'=[{100, ?COMMENT__POINTS_FOR_CONTRACT_MADE}], 'WE'=[]},
    TE11 = #score_entry{'WE'=[{120, ?COMMENT__POINTS_FOR_CONTRACT_MADE}], 'NS'=[]},
    TE12 = #score_entry{'WE'=[{150, ?COMMENT__POINTS_FOR_CONTRACT_MADE}], 'NS'=[]},
    TE13 = #score_entry{'WE'=[{300, ?COMMENT__POINTS_FOR_CONTRACT_MADE}], 'NS'=[]},
    TE14 = #score_entry{'WE'=[{100, ?COMMENT__POINTS_FOR_CONTRACT_MADE}], 'NS'=[]},
    [?_assertEqual('NS', bs_rubber_score:who_won_game(#score{below=[TE7]})),
    ?_assertEqual('NS', bs_rubber_score:who_won_game(#score{below=[TE8]})),
    ?_assertEqual('NS', bs_rubber_score:who_won_game(#score{below=[TE9]})),
    ?_assertEqual('NS', bs_rubber_score:who_won_game(#score{below=[TE10]})),
    ?_assertEqual('WE', bs_rubber_score:who_won_game(#score{below=[TE11]})),
    ?_assertEqual('WE', bs_rubber_score:who_won_game(#score{below=[TE12]})),
    ?_assertEqual('WE', bs_rubber_score:who_won_game(#score{below=[TE13]})),
    ?_assertEqual('WE', bs_rubber_score:who_won_game(#score{below=[TE14]})),
    ?_assertEqual('NS', bs_rubber_score:who_won_game(#score{below=[TE5, TE4, TE10]})),
    ?_assertEqual('WE', bs_rubber_score:who_won_game(#score{below=[TE6, TE4]})),
    ?_assertEqual('WE', bs_rubber_score:who_won_game(#score{below=[TE6, TE3, TE4]})),
    ?_assertEqual('NS', bs_rubber_score:who_won_game(#score{below=[TE3, TE6, TE8]}))
    ].


