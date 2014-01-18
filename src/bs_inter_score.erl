-module(bs_inter_score).

%% TODO 
%% -Change 'below' to 'below'
%% -Change naming from 'inter' or 'international' to 'rubber'
%% -Clean the code
%% -Separate test code

-include_lib("eunit/include/eunit.hrl").
-include("bs_data.hrl").

-export([process/3, process/4]).

-compile(export_all).

%%-----------------------------------------------------------------------------------------------
%% Comments added to scores entries
%%-----------------------------------------------------------------------------------------------
-define(COMMENT__POINTS_FOR_OPPONENTS_UNDERTRICKS, "Punkty za wpadki przeciwników").
-define(COMMENT__POINTS_FOR_CONTRACT_MADE, "Punkty za zrobiony kontrakt").
-define(COMMENT__OVERTRICKS_POINTS, "Punkty na nadróbki").
-define(COMMENT__SMALL_SLAM_BONUS, "Punkty za szlemika").
-define(COMMENT__GRAND_SLAM_BONUS, "Punkty za szlema").
-define(COMMENT__WON_DOUBLED_GAME, "Bonus za ugraną (re)kontrę").
-define(COMMENT__WINNING_RUBBER_BONUS, "Bonus za wygranie robra").
%%-----------------------------------------------------------------------------------------------
%% End of comments deffinitions
%%-----------------------------------------------------------------------------------------------
-define(POINTS_FOR_CLUBS_TRICK, 20).
-define(POINTS_FOR_DIMONDS_TRICK, 20).
-define(POINTS_FOR_HEARTS_TRICK, 30).
-define(POINTS_FOR_SPADES_TRICK, 30).
-define(POINTS_FOR_NOTRUMPH_TRICK, 30).
-define(POINTS_FOR_FIRST_NOTRUMPH_TRICK, 40).
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
    Score0 = #score{above=[], below=[], is_closed=false},
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
    [?_assertEqual(State1#game_state{score=Score1, round_no=2}, process(Contract1, Taken1, State1#game_state{score=Score0, round_no=1})),
    ?_assertEqual(State1#game_state{score=Score2, round_no=3}, process(Contract2, Taken2, State1#game_state{score=Score1, round_no=2})),
    ?_assertEqual(State1#game_state{score=Score3, round_no=4}, process(Contract3, Taken3, State1#game_state{score=Score2, round_no=3})),
    ?_assertEqual(State2#game_state{score=Score4, round_no=5}, process(Contract4, Taken4, State1#game_state{score=Score3, round_no=4})),
    ?_assertEqual(State2#game_state{score=Score5, round_no=6}, process(Contract5, Taken5, State2#game_state{score=Score4, round_no=5})),
    ?_assertEqual(State2#game_state{score=Score6, round_no=7}, process(Contract6, Taken6, State2#game_state{score=Score5, round_no=6})),
    ?_assertEqual(State2#game_state{score=Score7, round_no=8}, process(Contract7, Taken7, State2#game_state{score=Score6, round_no=7})),
    ?_assertEqual(State2#game_state{score=Score8, round_no=9}, process(Contract8, Taken8, State2#game_state{score=Score7, round_no=8})),
    ?_assertEqual(State3#game_state{score=Score9, round_no=10}, process(Contract9, Taken9, State2#game_state{score=Score8, round_no=9})),
    ?_assertEqual(State3#game_state{score=Score10, round_no=11}, process(Contract10, Taken10, State3#game_state{score=Score9, round_no=10})),
    ?_assertEqual(State3#game_state{score=Score11, round_no=12}, process(Contract11, Taken11, State3#game_state{score=Score10, round_no=11}))
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
    Score0 = #score{above=[], below=[], is_closed=false},
    Score1 = Score0#score{above=[[TE1A]], below=[TE1U]},
    Score2 = Score1#score{above=[[TE1A, TE2]]},
    Score3 = Score2#score{above=[[TE1A, TE2, TE3A], [TE1U, TE3U]], below=[]},    
    Score4 = Score3#score{below=[TE4]},    
    Score5 = Score4#score{above=[[TE1A, TE2, TE3A, TE5A], [TE1U, TE3U], [TE4, TE5U]],below=[]},    
    Score6 = Score5#score{above=[[TE1A, TE2, TE3A, TE5A, TE6], [TE1U, TE3U], [TE4, TE5U]]},    
    Score7 = Score6#score{above=[[TE1A, TE2, TE3A, TE5A, TE6], [TE1U, TE3U], [TE4, TE5U]], below=[TE7]},    
    Score8 = Score7#score{above=[[TE1A, TE2, TE3A, TE5A, TE6, TE8A], [TE1U, TE3U], [TE4, TE5U], [TE7, TE8U]], below=[], is_closed=true },    
    [
    ?_assertEqual(State1#game_state{score=Score1, round_no=2}, process(Contract1, Taken1, State1#game_state{score=Score0, round_no=1})),
    ?_assertEqual(State1#game_state{score=Score2, round_no=3}, process(Contract2, Taken2, State1#game_state{score=Score1, round_no=2})),
    ?_assertEqual(State2#game_state{score=Score3, round_no=4}, process(Contract3, Taken3, State1#game_state{score=Score2, round_no=3})),
    ?_assertEqual(State2#game_state{score=Score4, round_no=5}, process(Contract4, Taken4, State2#game_state{score=Score3, round_no=4})),
    ?_assertEqual(State3#game_state{score=Score5, round_no=6}, process(Contract5, Taken5, State2#game_state{score=Score4, round_no=5})),
    ?_assertEqual(State3#game_state{score=Score6, round_no=7}, process(Contract6, Taken6, State3#game_state{score=Score5, round_no=6})),
    ?_assertEqual(State3#game_state{score=Score7, round_no=8}, process(Contract7, Taken7, State3#game_state{score=Score6, round_no=7})),
    ?_assertEqual(State3#game_state{score=Score8, round_no=9}, process(Contract8, Taken8, State3#game_state{score=Score7, round_no=8}))
    ].


%%-----------------------------------------------------------------------------------------------
%% Function 'process'
%%-----------------------------------------------------------------------------------------------
process(Contract, Taken, State) ->
    process(Contract, Taken, State, []).

process(Contract, Taken, #game_state{round_no=RoundNo}=State, ExtraPoints) ->
    NewState = case is_contract_made(Contract, Taken) of
        true -> process_contract_made(Contract, Taken, State, ExtraPoints);
	false -> process_contract_not_made(Contract, Taken, State)
    end,
    NewState#game_state{round_no=RoundNo+1}.

%%--------------------------------------------------------------------------------------------
%% Tests for function 'is_contract_made'
%% test data contains all border cases and few other
%%--------------------------------------------------------------------------------------------
is_contract_made_test_() ->
    [?_assert(is_contract_made(#contract{level=1}, 7)),
    ?_assert(is_contract_made(#contract{level=2}, 8)),
    ?_assert(is_contract_made(#contract{level=3}, 9)),
    ?_assert(is_contract_made(#contract{level=4}, 10)),
    ?_assert(is_contract_made(#contract{level=5}, 11)),
    ?_assert(is_contract_made(#contract{level=6}, 12)),
    ?_assert(is_contract_made(#contract{level=7}, 13)),
    ?_assertNot(is_contract_made(#contract{level=1}, 6)),
    ?_assertNot(is_contract_made(#contract{level=2}, 7)),
    ?_assertNot(is_contract_made(#contract{level=3}, 8)),
    ?_assertNot(is_contract_made(#contract{level=4}, 9)),
    ?_assertNot(is_contract_made(#contract{level=5}, 10)),
    ?_assertNot(is_contract_made(#contract{level=6}, 11)),
    ?_assertNot(is_contract_made(#contract{level=7}, 12)),
    ?_assertNot(is_contract_made(#contract{level=4}, 8)),
    ?_assertNot(is_contract_made(#contract{level=5}, 8)),
    ?_assertNot(is_contract_made(#contract{level=6}, 9)),
    ?_assertNot(is_contract_made(#contract{level=5}, 10)),
    ?_assert(is_contract_made(#contract{level=2}, 10)),
    ?_assert(is_contract_made(#contract{level=3}, 10)),
    ?_assert(is_contract_made(#contract{level=3}, 11)),
    ?_assert(is_contract_made(#contract{level=4}, 12)),
    ?_assert(is_contract_made(#contract{level=4}, 11)),
    ?_assert(is_contract_made(#contract{level=5}, 13))
    ].


%%--------------------------------------------------------------------------------------------
%% Function checking if contract was fullfilled
%%--------------------------------------------------------------------------------------------
is_contract_made(#contract{level=Level}=_Contract, Taken) ->
    Level+6 =< Taken.

%%--------------------------------------------------------------------------------------------
%% Tests for function 'process_contract_not_made'
%%--------------------------------------------------------------------------------------------
process_contract_not_made_first_entry_test_() ->
    Contract = #contract{owner='NS', color='H', level=5},
    Taken = 8,
    State = #game_state{score=#score{above=[[]], below=[], is_closed=false}, is_NS_vulnerable=false, is_WE_vulnerable=false, round_no=1},
    State1 = State#game_state{is_NS_vulnerable=true},
    State2 = State#game_state{is_WE_vulnerable=true},
    Comment = ?COMMENT__POINTS_FOR_OPPONENTS_UNDERTRICKS,
    [?_assertEqual(State#game_state{score=#score{above=[[#score_entry{count=1, 'WE'=[{150, Comment}], 'NS'=[]}]], below=[], is_closed=false}}, process_contract_not_made(Contract, Taken, State)),
    ?_assertEqual(State1#game_state{score=#score{above=[[#score_entry{count=1, 'NS'=[], 'WE'=[{300, Comment}]}]], below=[], is_closed=false}}, process_contract_not_made(Contract, Taken, State1)),
    ?_assertEqual(State#game_state{score=#score{above=[[#score_entry{count=1, 'WE'=[], 'NS'=[{150, Comment}]}]], below=[], is_closed=false}}, process_contract_not_made(Contract#contract{owner='WE'}, Taken, State)),
    ?_assertEqual(State2#game_state{score=#score{above=[[#score_entry{count=1, 'WE'=[], 'NS'=[{300, Comment}]}]], below=[], is_closed=false}}, process_contract_not_made(Contract#contract{owner='WE'}, Taken, State2))
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
    Score0 = #score{above=[], below=[], is_closed=false},
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
    [?_assertEqual(State1#game_state{score=Score1},process_contract_not_made(Contract1, Taken1, State1#game_state{score=Score0, round_no=1})),
    ?_assertEqual(State1#game_state{score=Score2, round_no=2},process_contract_not_made(Contract2, Taken2, State1#game_state{score=Score1, round_no=2})),
    ?_assertEqual(State2#game_state{score=Score5, round_no=5},process_contract_not_made(Contract5, Taken5, State2#game_state{score=Score4, round_no=5})),
    ?_assertEqual(State2#game_state{score=Score6, round_no=6},process_contract_not_made(Contract6, Taken6, State2#game_state{score=Score5, round_no=6})),
    ?_assertEqual(State2#game_state{score=Score7, round_no=7},process_contract_not_made(Contract7, Taken7, State2#game_state{score=Score6, round_no=7})),
    ?_assertEqual(State2#game_state{score=Score8, round_no=8},process_contract_not_made(Contract8, Taken8, State2#game_state{score=Score7, round_no=8})),
    ?_assertEqual(State3#game_state{score=Score10, round_no=10},process_contract_not_made(Contract10, Taken10, State3#game_state{score=Score9, round_no=10})),
    ?_assertEqual(State3#game_state{score=Score11, round_no=11},process_contract_not_made(Contract11, Taken11, State3#game_state{score=Score10, round_no=11}))
    ].
    

%%--------------------------------------------------------------------------------------------
%% Function to prepare new score ranking in case that contract was now fullfilled
%%--------------------------------------------------------------------------------------------
process_contract_not_made(#contract{owner=Owner}=Contract, Taken, #game_state{score=Scores, round_no=RoundNo}=State) ->
    UndertricksPoints = count_belowtricks_scores(Contract, Taken, State),
    NewScoresEntry = create_score_entry_for_contract_not_made(Owner, UndertricksPoints, RoundNo),
    NewScores = insert_score_entry(above, Scores, NewScoresEntry),
    State#game_state{score=NewScores}.

%%--------------------------------------------------------------------------------------------------------------------------
%% Tests for counting scores for belowtricks
%%--------------------------------------------------------------------------------------------------------------------------
%% --- Regular ------------------------
count_belowtricks_scores_1before_test() ->
    Contract = #contract{owner='NS', level=3, doubled=false, redoubled=false},
    Taken = 8,
    State = #game_state{is_NS_vulnerable=false},
    Points = count_belowtricks_scores(Contract, Taken, State),
    ?_assert(Points =:= 50).
    
count_belowtricks_scores_5before_test() ->
    Contract = #contract{owner='NS', level=5, doubled=false, redoubled=false},
    Taken = 6,
    State = #game_state{is_NS_vulnerable=false},
    Points = count_belowtricks_scores(Contract, Taken, State),
    ?_assert(Points =:= 250).
    
count_belowtricks_scores_2after_test() ->
    Contract = #contract{owner='NS', level=4, doubled=false, redoubled=false},
    Taken = 8,
    State = #game_state{is_NS_vulnerable=true},
    Points = count_belowtricks_scores(Contract, Taken, State),
    ?_assert(Points =:= 200).

count_belowtricks_scores_6after_test() ->
    Contract = #contract{owner='NS', level=6, doubled=false, redoubled=false},
    Taken = 6,
    State = #game_state{is_NS_vulnerable=true},
    Points = count_belowtricks_scores(Contract, Taken, State),
    ?_assert(Points =:= 600).

%% --- Doubled -------------------------
count_belowtricks_scores_3before_dbl_test() ->
    Contract = #contract{owner='NS', level=7, doubled=true, redoubled=false},
    Taken = 10,
    State = #game_state{is_NS_vulnerable=false},
    Points = count_belowtricks_scores(Contract, Taken, State),
    ?_assert(Points =:= 500).

count_belowtricks_scores_5before_dbl_test() ->
    Contract = #contract{owner='NS', level=6, doubled=true, redoubled=false},
    Taken = 7,
    State = #game_state{is_NS_vulnerable=false},
    Points = count_belowtricks_scores(Contract, Taken, State),
    ?_assert(Points =:= 1100).

count_belowtricks_scores_1after_dbl_test() ->
    Contract = #contract{owner='NS', level=6, doubled=true, redoubled=false},
    Taken = 11,
    State = #game_state{is_NS_vulnerable=true},
    Points = count_belowtricks_scores(Contract, Taken, State),
    ?_assert(Points =:= 200).

count_belowtricks_scores_4after_dbl_test() ->
    Contract = #contract{owner='NS', level=5, doubled=true, redoubled=false},
    Taken = 7,
    State = #game_state{is_NS_vulnerable=true},
    Points = count_belowtricks_scores(Contract, Taken, State),
    ?_assert(Points =:= 1100).

%% --- Redoubled -----------------------
count_belowtricks_scores_3before_re_test() ->
    Contract = #contract{owner='NS', level=3, doubled=true, redoubled=true},
    Taken = 6,
    State = #game_state{is_NS_vulnerable=false},
    Points = count_belowtricks_scores(Contract, Taken, State),
    ?_assert(Points =:= 1000).

count_belowtricks_scores_7before_re_test() ->
    Contract = #contract{owner='NS', level=5, doubled=false, redoubled=true},
    Taken = 4,
    State = #game_state{is_NS_vulnerable=false},
    Points = count_belowtricks_scores(Contract, Taken, State),
    ?_assert(Points =:= 3400).

count_belowtricks_scores_2after_re_test() ->
    Contract = #contract{owner='NS', level=3, doubled=true, redoubled=true},
    Taken = 7,
    State = #game_state{is_NS_vulnerable=true},
    Points = count_belowtricks_scores(Contract, Taken, State),
    ?_assert(Points =:= 1000).

count_belowtricks_scores_5after_re_test() ->
    Contract = #contract{owner='NS', level=7, doubled=false, redoubled=true},
    Taken = 8,
    State = #game_state{is_NS_vulnerable=true},
    Points = count_belowtricks_scores(Contract, Taken, State),
    ?_assert(Points =:= 2800).


%%--------------------------------------------------------------------------------------------------------------------------
%% Function counting scors for belowtricks 
%%--------------------------------------------------------------------------------------------------------------------------
count_belowtricks_scores(#contract{owner='NS', level=Level, doubled=Dbl, redoubled=Re}=_Contract, Taken, #game_state{is_NS_vulnerable=false}=_State) ->
    count_belowtricks_score_not_vulnerable(Level+6-Taken, Dbl, Re);
count_belowtricks_scores(#contract{owner='NS', level=Level, doubled=Dbl, redoubled=Re}=_Contract, Taken, #game_state{is_NS_vulnerable=true}=_State) ->
    count_belowtricks_score_vulnerable(Level+6-Taken, Dbl, Re);
count_belowtricks_scores(#contract{owner='WE', level=Level, doubled=Dbl, redoubled=Re}=_Contract, Taken, #game_state{is_WE_vulnerable=false}=_State) ->
    count_belowtricks_score_not_vulnerable(Level+6-Taken, Dbl, Re);
count_belowtricks_scores(#contract{owner='WE', level=Level, doubled=Dbl, redoubled=Re}=_Contract, Taken, #game_state{is_WE_vulnerable=true}=_State) ->
    count_belowtricks_score_vulnerable(Level+6-Taken, Dbl, Re).

count_belowtricks_score_not_vulnerable(N, _Dbl, true) when N < 4 ->
    200+400*(N-1);
count_belowtricks_score_not_vulnerable(N, _Dbl, true) when N > 3 ->
    1000+600*(N-3);
count_belowtricks_score_not_vulnerable(N, true, _Re) when N < 4 ->
    100+200*(N-1);
count_belowtricks_score_not_vulnerable(N, true, _Re) when N > 3 ->
    500+300*(N-3);
count_belowtricks_score_not_vulnerable(N, false, false) ->
    50*N.  

count_belowtricks_score_vulnerable(N, _Dbl, true) ->
    400+600*(N-1);
count_belowtricks_score_vulnerable(N, true, _Re) ->
    200+300*(N-1);
count_belowtricks_score_vulnerable(N, false, false) ->
    100*N.  

%%---------------------------------------------------------------------------------------------------------------------------
%% End of counting score for belowtricks
%%---------------------------------------------------------------------------------------------------------------------------

%%---------------------------------------------------------------------------------------------------------------------------
%% Tests for function 'create_score_entry_for_contract_not_made'
%%---------------------------------------------------------------------------------------------------------------------------
create_score_entry_for_contract_not_made_test_() ->
    [?_assert(create_score_entry_for_contract_not_made('NS', 200, 1) == #score_entry{count=1, 'NS'=[], 'WE'=[{200, ?COMMENT__POINTS_FOR_OPPONENTS_UNDERTRICKS}]}),
    ?_assert(create_score_entry_for_contract_not_made('NS', 100, 3) == #score_entry{count=3, 'NS'=[], 'WE'=[{100, ?COMMENT__POINTS_FOR_OPPONENTS_UNDERTRICKS}]}),
    ?_assert(create_score_entry_for_contract_not_made('WE', 50, 4) == #score_entry{count=4, 'WE'=[], 'NS'=[{50, ?COMMENT__POINTS_FOR_OPPONENTS_UNDERTRICKS}]}),
    ?_assert(create_score_entry_for_contract_not_made('WE', 150, 8) == #score_entry{count=8, 'WE'=[], 'NS'=[{150, ?COMMENT__POINTS_FOR_OPPONENTS_UNDERTRICKS}]}),
    ?_assertException(error, function_clause, create_score_entry_for_contract_not_made(any, 100, 1))
    ].
%%---------------------------------------------------------------------------------------------------------------------------
%% Function for creation score_entries for failed contracts
%%---------------------------------------------------------------------------------------------------------------------------
create_score_entry_for_contract_not_made('NS', Points, RoundNo) ->
    #score_entry{count=RoundNo, 'WE'=[{Points, ?COMMENT__POINTS_FOR_OPPONENTS_UNDERTRICKS}]};
create_score_entry_for_contract_not_made('WE', Points, RoundNo) ->
    #score_entry{count=RoundNo, 'NS'=[{Points, ?COMMENT__POINTS_FOR_OPPONENTS_UNDERTRICKS}]}.

%%---------------------------------------------------------------------------------------------------------------------------
%% Tests for function 'create_score_entry_for_contract_made'
%%---------------------------------------------------------------------------------------------------------------------------
create_score_entry_for_contract_made_test_() ->
    [?_assert(create_score_entry_for_contract_made('NS', 200, 1) == #score_entry{count=1, 'WE'=[], 'NS'=[{200, ?COMMENT__POINTS_FOR_CONTRACT_MADE}]}),
    ?_assert(create_score_entry_for_contract_made('NS', 100, 3) == #score_entry{count=3, 'WE'=[], 'NS'=[{100, ?COMMENT__POINTS_FOR_CONTRACT_MADE}]}),
    ?_assert(create_score_entry_for_contract_made('WE', 50, 4) == #score_entry{count=4, 'NS'=[], 'WE'=[{50, ?COMMENT__POINTS_FOR_CONTRACT_MADE}]}),
    ?_assert(create_score_entry_for_contract_made('WE', 150, 8) == #score_entry{count=8, 'NS'=[], 'WE'=[{150, ?COMMENT__POINTS_FOR_CONTRACT_MADE}]}),
    ?_assertException(error, function_clause, create_score_entry_for_contract_made(any, 100, 1))
    ].
%%---------------------------------------------------------------------------------------------------------------------------
%% Function for creation score_entries for succeeded contracts
%%---------------------------------------------------------------------------------------------------------------------------
create_score_entry_for_contract_made('WE', Points, RoundNo) ->
    #score_entry{count=RoundNo, 'WE'=[{Points, ?COMMENT__POINTS_FOR_CONTRACT_MADE}]};
create_score_entry_for_contract_made('NS', Points, RoundNo) ->
    #score_entry{count=RoundNo, 'NS'=[{Points, ?COMMENT__POINTS_FOR_CONTRACT_MADE}]}.

%%---------------------------------------------------------------------------------------------------------------------------
%% Function for creation score_entries for bonuses
%%---------------------------------------------------------------------------------------------------------------------------
create_bonus_score_entry('NS', RoundNo, FinalBonuses) ->
    #score_entry{count=RoundNo, 'NS'=FinalBonuses, 'WE'=[]};    
create_bonus_score_entry('WE', RoundNo, FinalBonuses) ->
    #score_entry{count=RoundNo, 'WE'=FinalBonuses, 'NS'=[]}.    

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
    [?_assertEqual(#score{below=[TE1]}, insert_score_entry(below, #score{below=[]}, TE1)),
    ?_assertEqual(#score{below=[TE1,TE2,TE3,TE4]} ,insert_score_entry(below, #score{below=[TE1,TE2,TE3]}, TE4)), 
    ?_assertEqual(#score{above=[[TE1,TE2,TE3,TE4],[TE6]]}, insert_score_entry(above, #score{above=[[TE1,TE2,TE3],[TE6]]}, TE4)), 
    ?_assertEqual(#score{above=[[TE1,TE2,TE3,TE4]]},  insert_score_entry(above, #score{above=[[TE1,TE2,TE3]]}, TE4)), 
    ?_assertEqual(#score{above=[[TE1,TE2,TE4],[TE3]]}, insert_score_entry(above, #score{above=[[TE1,TE2],[TE3]]}, TE4)), 
    ?_assertEqual(#score{above=[[TE1,TE2,TE5],[TE3]], below=[TE4]}, insert_score_entry(above, #score{above=[[TE1,TE2],[TE3]], below=[TE4]}, TE5)),
    ?_assertException(error, function_clause, insert_score_entry(any, #score{}, TE1)),
    ?_assertEqual(#score{below=[]}, insert_score_entry(below, #score{below=[]}, ETE)),
    ?_assertEqual(#score{above=[]}, insert_score_entry(above, #score{above=[]}, ETE)),
    ?_assertEqual(#score{above=[[TE1,TE2,TE3]]},  insert_score_entry(above, #score{above=[[TE1,TE2,TE3]]}, ETE)), 
    ?_assertEqual(#score{below=[[TE1,TE2,TE3]]},  insert_score_entry(below, #score{below=[[TE1,TE2,TE3]]}, ETE)), 
    ?_assertEqual(#score{above=[[TE1,TE2],[TE3]], below=[TE4]}, insert_score_entry(above, #score{above=[[TE1,TE2],[TE3]], below=[TE4]}, ETE)),
    ?_assertEqual(#score{above=[[TE1,TE2],[TE3]], below=[TE4]}, insert_score_entry(below, #score{above=[[TE1,TE2],[TE3]], below=[TE4]}, ETE))
    ].
%%---------------------------------------------------------------------------------------------------------------------------
%% Function for inserting enitries into scoring 
%%---------------------------------------------------------------------------------------------------------------------------
insert_score_entry(_, Scores, #score_entry{'NS'=[], 'WE'=[]}) ->
    Scores;
insert_score_entry(above, #score{above=[]}=Scores, Entry) ->
    Scores#score{above=[[Entry]]};
insert_score_entry(above, #score{above=[Bonuses| ClosedGames]}=Scores, Entry) ->
    NewBonuses = case Bonuses of
        [] -> [Entry];
        _Other -> lists:reverse([Entry| lists:reverse(Bonuses)])
    end,
    Scores#score{above=[NewBonuses|ClosedGames]};
insert_score_entry(below, #score{below=Under}=Scores, Entry) ->
    NewUnder = lists:reverse([Entry| lists:reverse(Under)]),
    Scores#score{below=NewUnder}.


%%---------------------------------------------------------------------------------------------------------------------------
%% Tests for processing succesful contract 
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
    Score0 = #score{above=[], below=[], is_closed=false},
    Score1 = Score0#score{above=[[TE1A]], below=[TE1U]},
    Score3 = Score1#score{above=[[TE1A, TE3A], [TE1U, TE3U]], below=[]},    
    Score4 = Score3#score{below=[TE4]},    
    Score5 = Score4#score{above=[[TE1A, TE3A, TE5A], [TE1U, TE3U], [TE4, TE5U]],below=[]},    
    Score7 = Score5#score{above=[[TE1A, TE3A, TE5A], [TE1U, TE3U], [TE4, TE5U]], below=[TE7]},    
    Score8 = Score7#score{above=[[TE1A, TE3A, TE5A, TE8A], [TE1U, TE3U], [TE4, TE5U], [TE7, TE8U]], below=[], is_closed=true },    
    [
    ?_assertEqual(State1#game_state{score=Score1, round_no=1}, process_contract_made(Contract1, Taken1, State1#game_state{score=Score0, round_no=1}, [])),
    ?_assertEqual(State2#game_state{score=Score3, round_no=3}, process_contract_made(Contract3, Taken3, State1#game_state{score=Score1, round_no=3}, [])),
    ?_assertEqual(State2#game_state{score=Score4, round_no=4}, process_contract_made(Contract4, Taken4, State2#game_state{score=Score3, round_no=4}, [])),
    ?_assertEqual(State3#game_state{score=Score5, round_no=5}, process_contract_made(Contract5, Taken5, State2#game_state{score=Score4, round_no=5}, [])),
    ?_assertEqual(State3#game_state{score=Score7, round_no=7}, process_contract_made(Contract7, Taken7, State3#game_state{score=Score5, round_no=7}, [])),
    ?_assertEqual(State3#game_state{score=Score8, round_no=8}, process_contract_made(Contract8, Taken8, State3#game_state{score=Score7, round_no=8}, []))
    ].

%%---------------------------------------------------------------------------------------------------------------------------
%% Function for processing succesful contract 
%%---------------------------------------------------------------------------------------------------------------------------
process_contract_made(Contract, Taken, #game_state{score=PrevScore}=State, ExtraPoints) ->
    ScoreEntryForContract = count_scores_entry_for_contract(Contract, State), % count only score below line
    Temp1Score = insert_score_entry(below, PrevScore, ScoreEntryForContract),
    ScoreEntryForBonuses= create_score_entry_for_bonuses(Contract, Taken, State, ExtraPoints, Temp1Score), % count overtakes, bonuses for small and grand slam
    Temp2Score = insert_score_entry(above, Temp1Score, ScoreEntryForBonuses),
    _NewState = case is_game_won(Temp2Score) of
        false -> State#game_state{score=Temp2Score};
        true -> close_game(Temp2Score, State) % move score from below to above, check if rubber is done
    end.
        
count_scores_entry_for_contract(#contract{owner=Owner}=Contract, #game_state{round_no=RoundNo}=_State) ->
    Points = count_score_for_contract(Contract),
    _ScoreEntry = create_score_entry_for_contract_made(Owner, Points, RoundNo). 

%%---------------------------------------------------------------------------------------------------------------------------
%% Tests for counting scores for contract made
%%---------------------------------------------------------------------------------------------------------------------------
count_score_for_contract_test_() ->
    [?_assertEqual(120, count_score_for_contract(#contract{color='C', level=6, doubled=false})),
    ?_assertEqual(120, count_score_for_contract(#contract{color='C', level=3, doubled=true})),
    ?_assertEqual(100, count_score_for_contract(#contract{color='C', level=5})),
    ?_assertEqual(40, count_score_for_contract(#contract{color='C', level=2})),
    ?_assertEqual(160, count_score_for_contract(#contract{color='C', level=2, redoubled=true})),
    ?_assertEqual(80, count_score_for_contract(#contract{color='D', level=4})),
    ?_assertEqual(160, count_score_for_contract(#contract{color='D', level=4, doubled=true})),
    ?_assertEqual(240, count_score_for_contract(#contract{color='D', level=3, redoubled=true})),
    ?_assertEqual(60, count_score_for_contract(#contract{color='H', level=2})),
    ?_assertEqual(30, count_score_for_contract(#contract{color='H', level=1})),
    ?_assertEqual(60, count_score_for_contract(#contract{color='H', level=1, doubled=true})),
    ?_assertEqual(120, count_score_for_contract(#contract{color='H', level=1, redoubled=true})),
    ?_assertEqual(150, count_score_for_contract(#contract{color='H', level=5})),
    ?_assertEqual(150, count_score_for_contract(#contract{color='S', level=5})),
    ?_assertEqual(300, count_score_for_contract(#contract{color='S', level=5, doubled=true})),
    ?_assertEqual(180, count_score_for_contract(#contract{color='S', level=3, doubled=true})),
    ?_assertEqual(240, count_score_for_contract(#contract{color='S', level=2, redoubled=true})),
    ?_assertEqual(40, count_score_for_contract(#contract{color='N', level=1})),
    ?_assertEqual(70, count_score_for_contract(#contract{color='N', level=2})),
    ?_assertEqual(100, count_score_for_contract(#contract{color='N', level=3})),
    ?_assertEqual(200, count_score_for_contract(#contract{color='N', level=3, doubled=true})),
    ?_assertEqual(140, count_score_for_contract(#contract{color='N', level=2, doubled=true})),
    ?_assertEqual(640, count_score_for_contract(#contract{color='N', level=5, doubled=true, redoubled=true}))
    ].

%%---------------------------------------------------------------------------------------------------------------------------
%% Function counting scores for contract made
%%---------------------------------------------------------------------------------------------------------------------------
count_score_for_contract(#contract{color='C', level=Level, doubled=Dbl, redoubled=Re}) ->
    Base = Level*?POINTS_FOR_CLUBS_TRICK,
    apply_doubled_redoubled(Base, Dbl, Re);
count_score_for_contract(#contract{color='D', level=Level, doubled=Dbl, redoubled=Re}) ->
    Base = Level*?POINTS_FOR_DIMONDS_TRICK,
    apply_doubled_redoubled(Base, Dbl, Re);
count_score_for_contract(#contract{color='H', level=Level, doubled=Dbl, redoubled=Re}) ->
    Base = Level*?POINTS_FOR_HEARTS_TRICK,
    apply_doubled_redoubled(Base, Dbl, Re);
count_score_for_contract(#contract{color='S', level=Level, doubled=Dbl, redoubled=Re}) ->
    Base = Level*?POINTS_FOR_SPADES_TRICK,
    apply_doubled_redoubled(Base, Dbl, Re);
count_score_for_contract(#contract{color='N', level=Level, doubled=Dbl, redoubled=Re}) ->
    Base = ?POINTS_FOR_FIRST_NOTRUMPH_TRICK+(Level-1)*?POINTS_FOR_NOTRUMPH_TRICK,
    apply_doubled_redoubled(Base, Dbl, Re).

apply_doubled_redoubled(Base, _Dbl, true) ->
    Base*4;
apply_doubled_redoubled(Base, true, false) ->
    Base*2;
apply_doubled_redoubled(Base, false, false) ->
    Base.

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
    [?_assertEqual(SE1, create_score_entry_for_bonuses(Contract1, Taken1, State1, [], #score{})),
    ?_assertEqual(SE2, create_score_entry_for_bonuses(Contract2, Taken2, State1, [], #score{})),
    ?_assertEqual(SE3, create_score_entry_for_bonuses(Contract3, Taken3, State1, [], #score{})),
    ?_assertEqual(SE4, create_score_entry_for_bonuses(Contract4, Taken4, State1, [], #score{})),
    ?_assertEqual(SE5, create_score_entry_for_bonuses(Contract5, Taken5, State1, [], #score{})),
    ?_assertEqual(SE6, create_score_entry_for_bonuses(Contract6, Taken6, State1, [], #score{})),
    ?_assertEqual(SE7, create_score_entry_for_bonuses(Contract7, Taken7, State1, [], #score{})),
    ?_assertEqual(SE1, create_score_entry_for_bonuses(Contract1, Taken1, State2, [], #score{})),
    ?_assertEqual(SE2, create_score_entry_for_bonuses(Contract2, Taken2, State2, [], #score{})),
    ?_assertEqual(SE3, create_score_entry_for_bonuses(Contract3, Taken3, State2, [], #score{})),
    ?_assertEqual(SE42, create_score_entry_for_bonuses(Contract4, Taken4, State2, [], #score{})),
    ?_assertEqual(SE52, create_score_entry_for_bonuses(Contract5, Taken5, State2, [], #score{})),
    ?_assertEqual(SE6, create_score_entry_for_bonuses(Contract6, Taken6, State2, [], #score{})),
    ?_assertEqual(SE72, create_score_entry_for_bonuses(Contract7, Taken7, State2, [], #score{}))
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
    [?_assertEqual(#score_entry{count=1, 'NS'=[Score1], 'WE'=[]}, create_score_entry_for_bonuses(Contract1, 12, State1, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'NS'=[{20, ?COMMENT__OVERTRICKS_POINTS}, Score1], 'WE'=[]}, create_score_entry_for_bonuses(Contract1, 13, State1, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'NS'=[Score3], 'WE'=[]}, create_score_entry_for_bonuses(Contract2, 13, State1, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'NS'=[Score2], 'WE'=[]}, create_score_entry_for_bonuses(Contract1, 12, State2, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'NS'=[{20, ?COMMENT__OVERTRICKS_POINTS}, Score2], 'WE'=[]}, create_score_entry_for_bonuses(Contract1, 13, State2, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'NS'=[Score4], 'WE'=[]}, create_score_entry_for_bonuses(Contract2, 13, State2, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'NS'=[Score1, Score5], 'WE'=[]}, create_score_entry_for_bonuses(Contract3, 12, State1, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'NS'=[{100, ?COMMENT__OVERTRICKS_POINTS}, Score1, Score5], 'WE'=[]}, create_score_entry_for_bonuses(Contract3, 13, State1, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'NS'=[Score3, Score5], 'WE'=[]}, create_score_entry_for_bonuses(Contract4, 13, State1, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'NS'=[Score2, Score5], 'WE'=[]}, create_score_entry_for_bonuses(Contract3, 12, State2, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'NS'=[{200, ?COMMENT__OVERTRICKS_POINTS}, Score2, Score5], 'WE'=[]}, create_score_entry_for_bonuses(Contract3, 13, State2, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'NS'=[Score4, Score5], 'WE'=[]}, create_score_entry_for_bonuses(Contract4, 13, State2, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'NS'=[Score1, Score5], 'WE'=[]}, create_score_entry_for_bonuses(Contract5, 12, State1, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'NS'=[{200, ?COMMENT__OVERTRICKS_POINTS}, Score1, Score5], 'WE'=[]}, create_score_entry_for_bonuses(Contract5, 13, State1, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'NS'=[Score3, Score5], 'WE'=[]}, create_score_entry_for_bonuses(Contract6, 13, State1, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'NS'=[Score2, Score5], 'WE'=[]}, create_score_entry_for_bonuses(Contract5, 12, State2, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'NS'=[{400, ?COMMENT__OVERTRICKS_POINTS}, Score2, Score5], 'WE'=[]}, create_score_entry_for_bonuses(Contract5, 13, State2, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'NS'=[Score4, Score5], 'WE'=[]}, create_score_entry_for_bonuses(Contract6, 13, State2, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'WE'=[Score1], 'NS'=[]}, create_score_entry_for_bonuses(Contract7, 12, State1, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'WE'=[{30, ?COMMENT__OVERTRICKS_POINTS}, Score1], 'NS'=[]}, create_score_entry_for_bonuses(Contract7, 13, State1, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'WE'=[Score3], 'NS'=[]}, create_score_entry_for_bonuses(Contract8, 13, State1, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'WE'=[Score2], 'NS'=[]}, create_score_entry_for_bonuses(Contract7, 12, State3, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'WE'=[{30, ?COMMENT__OVERTRICKS_POINTS}, Score2], 'NS'=[]}, create_score_entry_for_bonuses(Contract7, 13, State3, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'WE'=[Score4], 'NS'=[]}, create_score_entry_for_bonuses(Contract8, 13, State3, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'WE'=[Score1, Score5], 'NS'=[]}, create_score_entry_for_bonuses(Contract9, 12, State1, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'WE'=[{100, ?COMMENT__OVERTRICKS_POINTS}, Score1, Score5], 'NS'=[]}, create_score_entry_for_bonuses(Contract9, 13, State1, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'WE'=[Score3, Score5], 'NS'=[]}, create_score_entry_for_bonuses(Contract10, 13, State1, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'WE'=[Score2, Score5], 'NS'=[]}, create_score_entry_for_bonuses(Contract9, 12, State3, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'WE'=[{200, ?COMMENT__OVERTRICKS_POINTS}, Score2, Score5], 'NS'=[]}, create_score_entry_for_bonuses(Contract9, 13, State3, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'WE'=[Score4, Score5], 'NS'=[]}, create_score_entry_for_bonuses(Contract10, 13, State3, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'WE'=[Score1, Score5], 'NS'=[]}, create_score_entry_for_bonuses(Contract11, 12, State1, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'WE'=[{200, ?COMMENT__OVERTRICKS_POINTS}, Score1, Score5], 'NS'=[]}, create_score_entry_for_bonuses(Contract11, 13, State1, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'WE'=[Score3, Score5], 'NS'=[]}, create_score_entry_for_bonuses(Contract12, 13, State1, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'WE'=[Score2, Score5], 'NS'=[]}, create_score_entry_for_bonuses(Contract11, 12, State3, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'WE'=[{400, ?COMMENT__OVERTRICKS_POINTS}, Score2, Score5], 'NS'=[]}, create_score_entry_for_bonuses(Contract11, 13, State3, [], #score{})),
    ?_assertEqual(#score_entry{count=1, 'WE'=[Score4, Score5], 'NS'=[]}, create_score_entry_for_bonuses(Contract12, 13, State3, [], #score{}))
    ].
    
create_score_entry_for_bonuses_rubber_won_test_() ->
    [].

create_score_entry_for_bonuses_extra_points_test_() ->
    [].

%%---------------------------------------------------------------------------------------------------------------------------
%% Function for counting bonus points
%%---------------------------------------------------------------------------------------------------------------------------
create_score_entry_for_bonuses(#contract{owner=Owner}=Contract, Taken, #game_state{round_no=RoundNo}=State, ExtraPoints, TempScore) ->
    Bonuses = create_bonuses_list(Contract, Taken, State, TempScore),
    FinalBonuses = case ExtraPoints of
        [] -> Bonuses;
        _Other -> [Bonuses|ExtraPoints]
    end,
    _ScoreEntry = create_bonus_score_entry(Owner, RoundNo, FinalBonuses).

create_bonuses_list(#contract{owner=Owner}=Contract, Taken, #game_state{is_NS_vulnerable=NSVulnerable, is_WE_vulnerable=WEVulnerable}=_State, TempScore) ->
    Overtricks = count_overtricks_points(Contract, Taken, is_vulnerable(Owner, NSVulnerable, WEVulnerable)),
    Slam = count_slam_bonus(Contract, is_vulnerable(Owner, NSVulnerable, WEVulnerable)),
    DoubledDone = count_double_done_bonus(Contract),
    RubberWon = count_rubber_won_bonus(TempScore, is_vulnerable(Owner, NSVulnerable, WEVulnerable), is_vulnerable(get_opponent(Owner), NSVulnerable, WEVulnerable)),
    lists:filter(fun(X) -> case X of no_points -> false; _ -> true end end,[Overtricks, Slam, DoubledDone, RubberWon]).

is_vulnerable('NS', NSVulnerable, _WEVulnerable) ->
    NSVulnerable;
is_vulnerable('WE', _NSVulnerable, WEVulnerable) ->
    WEVulnerable.

set_vulnerable('NS', State) ->
    State#game_state{is_NS_vulnerable=true};
set_vulnerable('WE', State) ->
    State#game_state{is_WE_vulnerable=true}.

get_opponent('NS') -> 'WE';
get_opponent('WE') -> 'NS'.

count_overtricks_points(#contract{color=Color, level=Level, doubled=Dbl, redoubled=Re}=_Contract, Taken, Vulnerable) ->
    OvertricksCount = Taken-6-Level,
    count_overtricks_points(OvertricksCount, Color, Dbl, Re, Vulnerable).
   
count_overtricks_points(0, _, _, _, _) ->
    no_points;
count_overtricks_points(Count, _Color, _Dbl, true, true) ->
    {400*Count, ?COMMENT__OVERTRICKS_POINTS};
count_overtricks_points(Count, _Color, true, false, true) ->
    {200*Count, ?COMMENT__OVERTRICKS_POINTS};  
count_overtricks_points(Count, _Color, _Dbl, true, false) ->
    {200*Count, ?COMMENT__OVERTRICKS_POINTS};  
count_overtricks_points(Count, _Color, true, false, false) ->
    {100*Count, ?COMMENT__OVERTRICKS_POINTS};  
count_overtricks_points(Count, 'C', false, false, _Vulnerable) ->
    {20*Count, ?COMMENT__OVERTRICKS_POINTS};  
count_overtricks_points(Count, 'D', false, false, _Vulnerable) ->
    {20*Count, ?COMMENT__OVERTRICKS_POINTS};  
count_overtricks_points(Count, 'H', false, false, _Vulnerable) ->
    {30*Count, ?COMMENT__OVERTRICKS_POINTS};  
count_overtricks_points(Count, 'S', false, false, _Vulnerable) ->
    {30*Count, ?COMMENT__OVERTRICKS_POINTS};  
count_overtricks_points(Count, 'N', false, false, _Vulnerable) ->
    {40+30*(Count-1), ?COMMENT__OVERTRICKS_POINTS}.

count_slam_bonus(#contract{level=6}=_Contract, Vulnerable) ->
    case Vulnerable of
        true ->  {750, ?COMMENT__SMALL_SLAM_BONUS};
        false -> {500, ?COMMENT__SMALL_SLAM_BONUS}
    end;
count_slam_bonus(#contract{level=7}=_Contract, Vulnerable) ->
    case Vulnerable of
        true -> {1500, ?COMMENT__GRAND_SLAM_BONUS};
        false -> {1000, ?COMMENT__GRAND_SLAM_BONUS}
    end;
count_slam_bonus(_Contract, _Vulnerable) ->
    no_points.

count_double_done_bonus(#contract{doubled=true})->
    {50, ?COMMENT__WON_DOUBLED_GAME};
count_double_done_bonus(#contract{redoubled=true})->
    {50, ?COMMENT__WON_DOUBLED_GAME};
count_double_done_bonus(_)->
    no_points.

count_rubber_won_bonus(_TempScore, false, _OpponentVulnerable) ->
    % Cannot win rubber if was not vulnerable 
    no_points;
count_rubber_won_bonus(TempScore, true, OpponentVulnerable) ->
    case is_game_won(TempScore) of
        false -> no_points;
        true -> 
            case OpponentVulnerable of
                true -> {500, ?COMMENT__WINNING_RUBBER_BONUS};
                false -> {700, ?COMMENT__WINNING_RUBBER_BONUS}
            end
    end.


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
    [?_assertNot(is_game_won(#score{below=[]})), 
    ?_assertNot(is_game_won(#score{below=[TE1]})),
    ?_assertNot(is_game_won(#score{below=[TE2]})),
    ?_assertNot(is_game_won(#score{below=[TE3]})),
    ?_assertNot(is_game_won(#score{below=[TE1, TE1]})),
    ?_assertNot(is_game_won(#score{below=[TE1, TE2]})),
    ?_assertNot(is_game_won(#score{below=[TE1, TE1, TE1]})),
    ?_assertNot(is_game_won(#score{below=[TE4]})),
    ?_assertNot(is_game_won(#score{below=[TE5]})),
    ?_assertNot(is_game_won(#score{below=[TE6]})),
    ?_assertNot(is_game_won(#score{below=[TE4, TE4]})),
    ?_assertNot(is_game_won(#score{below=[TE4, TE5]})),
    ?_assertNot(is_game_won(#score{below=[TE4, TE4, TE4]})),
    ?_assertNot(is_game_won(#score{below=[TE1, TE5]})),
    ?_assertNot(is_game_won(#score{below=[TE2, TE4]})),
    ?_assertNot(is_game_won(#score{below=[TE3, TE6]})),
    ?_assertNot(is_game_won(#score{below=[TE1, TE1, TE5, TE4]})),
    ?_assertNot(is_game_won(#score{below=[TE1, TE2, TE4, TE4]})),
    ?_assertNot(is_game_won(#score{below=[TE1, TE1, TE1, TE6]})),
    ?_assert(is_game_won(#score{below=[TE7]})),
    ?_assert(is_game_won(#score{below=[TE8]})),
    ?_assert(is_game_won(#score{below=[TE9]})),
    ?_assert(is_game_won(#score{below=[TE10]})),
    ?_assert(is_game_won(#score{below=[TE11]})),
    ?_assert(is_game_won(#score{below=[TE12]})),
    ?_assert(is_game_won(#score{below=[TE13]})),
    ?_assert(is_game_won(#score{below=[TE14]})),
    ?_assert(is_game_won(#score{below=[TE5, TE4, TE10]})),
    ?_assert(is_game_won(#score{below=[TE6, TE4]})),
    ?_assert(is_game_won(#score{below=[TE6, TE3, TE4]})),
    ?_assert(is_game_won(#score{below=[TE3, TE6, TE8]}))
    ].
%%---------------------------------------------------------------------------------------------------------------------------
%% Function checking if one game in rubber is finished
%%---------------------------------------------------------------------------------------------------------------------------
is_game_won(#score{below=Points}) ->
    {_, Sum} = sum_points(Points),
    Sum >= 100.

sum_points(Points) ->
    sum_points(Points, 0, 0).

sum_points([], AccNS, AccWE) ->
    case AccNS > AccWE of
        true -> {'NS', AccNS};
        false -> {'WE', AccWE}
    end;
sum_points([#score_entry{'NS'=[], 'WE'=Points}| T], AccNS, AccWE) ->
    NewAccWE = AccWE + sum_one_entry(Points),
    sum_points(T, AccNS, NewAccWE);
sum_points([#score_entry{'NS'=Points, 'WE'=[]}|T], AccNS, AccWE) ->
    NewAccNS = AccNS + sum_one_entry(Points),
    sum_points(T, NewAccNS, AccWE).

sum_one_entry(Points) ->
    sum_one_entry(Points, 0).

sum_one_entry([], Acc) ->
    Acc;
sum_one_entry([{Points, _}|T], Acc) ->
    sum_one_entry(T, Acc+Points).

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
    ExpScore1 = #score{above=[[TE1A],[TE1U]], below=[], is_closed=false},
    State1 = #game_state{score=Score1, is_NS_vulnerable=false, is_WE_vulnerable=false},
    ExpState1 = #game_state{score=ExpScore1, is_NS_vulnerable=false, is_WE_vulnerable=true},
    Score2 = #score{above=[[TE2A]], below=[TE2U]},
    ExpScore2 = #score{above=[[TE2A],[TE2U]], below=[], is_closed=false},
    State2 = #game_state{score=Score2, is_NS_vulnerable=false, is_WE_vulnerable=false},
    ExpState2 = #game_state{score=ExpScore2, is_NS_vulnerable=true, is_WE_vulnerable=false},
    Score3 = #score{above=[[TE6, TE1A]], below=[TE1U]},
    ExpScore3 = #score{above=[[TE6, TE1A],[TE1U]], below=[], is_closed=false},
    State3 = #game_state{score=Score3, is_NS_vulnerable=true, is_WE_vulnerable=false},
    ExpState3 = #game_state{score=ExpScore3, is_NS_vulnerable=true, is_WE_vulnerable=true},
    Score4 = #score{above=[[TE6, TE2A]], below=[TE2U]},
    ExpScore4 = #score{above=[[TE6, TE2A],[TE2U]], below=[], is_closed=false},
    State4 = #game_state{score=Score4, is_NS_vulnerable=false, is_WE_vulnerable=true},
    ExpState4 = #game_state{score=ExpScore4, is_NS_vulnerable=true, is_WE_vulnerable=true},
    Score5 = #score{above=[[TE6, TE1A]], below=[TE1U]},
    ExpScore5 = #score{above=[[TE6, TE1A],[TE1U]], below=[], is_closed=true},
    State5 = #game_state{score=Score5, is_NS_vulnerable=true, is_WE_vulnerable=true},
    ExpState5 = #game_state{score=ExpScore5, is_NS_vulnerable=true, is_WE_vulnerable=true},
    Score6 = #score{above=[[TE6, TE2A]], below=[TE2U]},
    ExpScore6 = #score{above=[[TE6, TE2A],[TE2U]], below=[], is_closed=true},
    State6 = #game_state{score=Score6, is_NS_vulnerable=true, is_WE_vulnerable=true},
    ExpState6 = #game_state{score=ExpScore6, is_NS_vulnerable=true, is_WE_vulnerable=true},
    Score7 = #score{above=[[]], below=[TE1U]},
    ExpScore7 = #score{above=[[],[TE1U]], below=[], is_closed=false},
    State7 = #game_state{score=Score7, is_NS_vulnerable=false, is_WE_vulnerable=false},
    ExpState7 = #game_state{score=ExpScore7, is_NS_vulnerable=false, is_WE_vulnerable=true},
    [?_assertEqual(ExpState1, close_game(Score1, State1)),
    ?_assertEqual(ExpState2, close_game(Score2, State2)),
    ?_assertEqual(ExpState3, close_game(Score3, State3)),
    ?_assertEqual(ExpState4, close_game(Score4, State4)),
    ?_assertEqual(ExpState5, close_game(Score5, State5)),
    ?_assertEqual(ExpState6, close_game(Score6, State6)),
    ?_assertEqual(ExpState7, close_game(Score7, State7))
    ].
%%---------------------------------------------------------------------------------------------------------------------------
%% Function moves points from below line to above, sets vulnerability and if rubbed is done marks scoring as closed
%%---------------------------------------------------------------------------------------------------------------------------
close_game(Score, State) ->
    NewScore = move_points_from_below_to_above(Score),
    Winner = who_won_game(Score),
    RetState = case is_rubber_done(Winner, State) of
        false -> State#game_state{score=NewScore}; 
        true -> State#game_state{score=NewScore#score{is_closed=true}} 
    end,
    set_vulnerable(Winner, RetState).

%%---------------------------------------------------------------------------------------------------------------------------
%% Test for moving scores from below to above
%%---------------------------------------------------------------------------------------------------------------------------
move_points_from_below_to_abve_test_() ->
    [?_assertEqual(#score{above=[[bonuses],[points_from_below]], below=[]}, move_points_from_below_to_above(#score{above=[[bonuses]], below=[points_from_below]})),
    ?_assertEqual(#score{above=[[bonuses],[points_from_first_game],[points_from_last_game]], below=[]}, move_points_from_below_to_above(#score{above=[[bonuses],[points_from_first_game]], below=[points_from_last_game]})),
    ?_assertEqual(#score{above=[[],[points_from_below]], below=[]}, move_points_from_below_to_above(#score{above=[[]], below=[points_from_below]}))
    ].
%%---------------------------------------------------------------------------------------------------------------------------
%% Function moving scores from below to above
%%---------------------------------------------------------------------------------------------------------------------------
move_points_from_below_to_above(#score{above=Above, below=Under}=Score) ->
    NewAbove = lists:reverse([Under| lists:reverse(Above)]),
    Score#score{above=NewAbove, below=[]}.

is_rubber_done(Winner, #game_state{is_NS_vulnerable=NSVulnerable, is_WE_vulnerable=WEVulnerable}=_State) ->
    is_vulnerable(Winner, NSVulnerable, WEVulnerable).


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
    [?_assertEqual('NS', who_won_game(#score{below=[TE7]})),
    ?_assertEqual('NS', who_won_game(#score{below=[TE8]})),
    ?_assertEqual('NS', who_won_game(#score{below=[TE9]})),
    ?_assertEqual('NS', who_won_game(#score{below=[TE10]})),
    ?_assertEqual('WE', who_won_game(#score{below=[TE11]})),
    ?_assertEqual('WE', who_won_game(#score{below=[TE12]})),
    ?_assertEqual('WE', who_won_game(#score{below=[TE13]})),
    ?_assertEqual('WE', who_won_game(#score{below=[TE14]})),
    ?_assertEqual('NS', who_won_game(#score{below=[TE5, TE4, TE10]})),
    ?_assertEqual('WE', who_won_game(#score{below=[TE6, TE4]})),
    ?_assertEqual('WE', who_won_game(#score{below=[TE6, TE3, TE4]})),
    ?_assertEqual('NS', who_won_game(#score{below=[TE3, TE6, TE8]}))
    ].


%%---------------------------------------------------------------------------------------------------------------------------
%% Function checking who won the game
%%---------------------------------------------------------------------------------------------------------------------------
who_won_game(#score{below=Under}) ->
    {Winner, _Points} = sum_points(Under),
    Winner.
