-module(bs_rubber_score).

%% TODO 
%% -Clean the code

-include("bs_data.hrl").
-include("bs_lang.hrl").

-export([process/3, process/4]).

-ifdef(TEST).
-compile(export_all).
-endif.

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
%% Function checking if contract was fullfilled
%%--------------------------------------------------------------------------------------------
is_contract_made(#contract{level=Level}=_Contract, Taken) ->
    Level+6 =< Taken.

%%--------------------------------------------------------------------------------------------
%% Function to prepare new score ranking in case that contract was now fullfilled
%%--------------------------------------------------------------------------------------------
process_contract_not_made(#contract{owner=Owner}=Contract, Taken, #game_state{score=Scores, round_no=RoundNo}=State) ->
    UndertricksPoints = count_undertricks_scores(Contract, Taken, State),
    NewScoresEntry = create_score_entry_for_contract_not_made(Owner, UndertricksPoints, RoundNo),
    NewScores = insert_score_entry(above, Scores, NewScoresEntry),
    State#game_state{score=NewScores}.

%%--------------------------------------------------------------------------------------------------------------------------
%% Function counting scors for undertricks 
%%--------------------------------------------------------------------------------------------------------------------------
count_undertricks_scores(#contract{owner='NS', level=Level, doubled=Dbl, redoubled=Re}=_Contract, Taken, #game_state{is_NS_vulnerable=false}=_State) ->
    count_undertricks_score_not_vulnerable(Level+6-Taken, Dbl, Re);
count_undertricks_scores(#contract{owner='NS', level=Level, doubled=Dbl, redoubled=Re}=_Contract, Taken, #game_state{is_NS_vulnerable=true}=_State) ->
    count_undertricks_score_vulnerable(Level+6-Taken, Dbl, Re);
count_undertricks_scores(#contract{owner='WE', level=Level, doubled=Dbl, redoubled=Re}=_Contract, Taken, #game_state{is_WE_vulnerable=false}=_State) ->
    count_undertricks_score_not_vulnerable(Level+6-Taken, Dbl, Re);
count_undertricks_scores(#contract{owner='WE', level=Level, doubled=Dbl, redoubled=Re}=_Contract, Taken, #game_state{is_WE_vulnerable=true}=_State) ->
    count_undertricks_score_vulnerable(Level+6-Taken, Dbl, Re).

count_undertricks_score_not_vulnerable(N, _Dbl, true) when N < 4 ->
    200+400*(N-1);
count_undertricks_score_not_vulnerable(N, _Dbl, true) when N > 3 ->
    1000+600*(N-3);
count_undertricks_score_not_vulnerable(N, true, _Re) when N < 4 ->
    100+200*(N-1);
count_undertricks_score_not_vulnerable(N, true, _Re) when N > 3 ->
    500+300*(N-3);
count_undertricks_score_not_vulnerable(N, false, false) ->
    50*N.  

count_undertricks_score_vulnerable(N, _Dbl, true) ->
    400+600*(N-1);
count_undertricks_score_vulnerable(N, true, _Re) ->
    200+300*(N-1);
count_undertricks_score_vulnerable(N, false, false) ->
    100*N.  

%%---------------------------------------------------------------------------------------------------------------------------
%% End of counting score for undertricks
%%---------------------------------------------------------------------------------------------------------------------------

%%---------------------------------------------------------------------------------------------------------------------------
%% Function for creation score_entries for failed contracts
%%---------------------------------------------------------------------------------------------------------------------------
create_score_entry_for_contract_not_made('NS', Points, RoundNo) ->
    #score_entry{count=RoundNo, 'WE'=[{Points, ?COMMENT__POINTS_FOR_OPPONENTS_UNDERTRICKS}]};
create_score_entry_for_contract_not_made('WE', Points, RoundNo) ->
    #score_entry{count=RoundNo, 'NS'=[{Points, ?COMMENT__POINTS_FOR_OPPONENTS_UNDERTRICKS}]}.

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
%% Function for processing succesful contract 
%%---------------------------------------------------------------------------------------------------------------------------
process_contract_made(Contract, Taken, #game_state{score=PrevScore}=State, ExtraPoints) ->
    ScoreEntryForContract = count_scores_entry_for_contract(Contract, State), % count only score below line
    Temp1Score = insert_score_entry(below, PrevScore, ScoreEntryForContract),
    ScoreEntryForBonuses= create_score_entry_for_bonuses(Contract, Taken, State, ExtraPoints, Temp1Score), % count overtakes, bonuses for small and grand slam
    Temp2Score = insert_score_entry(above, Temp1Score, ScoreEntryForBonuses),
    NewState = State#game_state{score=Temp2Score},
    case is_game_won(Temp2Score) of
        false -> NewState;
        true -> close_game(NewState) % move score from below to above, check if rubber is done
    end.
        
count_scores_entry_for_contract(#contract{owner=Owner}=Contract, #game_state{round_no=RoundNo}=_State) ->
    Points = count_score_for_contract(Contract),
    _ScoreEntry = create_score_entry_for_contract_made(Owner, Points, RoundNo). 

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
%% Function checking if one game in rubber is finished
%%---------------------------------------------------------------------------------------------------------------------------
is_game_won(#score{below=Points}) ->
    {{'NS', AccNS}, {'WE', AccWE}} = sum_points(Points),
    case {AccNS >= 100, AccWE >= 100} of
        {true, _}   -> true;
        {_, true}   -> true;
        _           -> false
    end.

%% ========
sum_points(Points) ->
    sum_points(Points, 0, 0).

sum_points([], AccNS, AccWE) ->
    {{'NS', AccNS}, {'WE', AccWE}};
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
%% Function moves points from below line to above, sets vulnerability and if rubbed is done marks scoring as closed
%%---------------------------------------------------------------------------------------------------------------------------
close_game(State) ->
    Winner = who_won_game(State#game_state.score),
    NewScore = move_points_from_below_to_above(State#game_state.score),
    Summary = case is_rubber_done(Winner, State) of
        false -> unfinished;
        true -> create_summary(NewScore)
    end,
    RetState = State#game_state{score=NewScore, status=Summary},
    set_vulnerable(Winner, RetState).

%%---------------------------------------------------------------------------------------------------------------------------
%% Function moving scores from below to above
%%---------------------------------------------------------------------------------------------------------------------------
move_points_from_below_to_above(#score{above=Above, below=Under}=Score) ->
    NewAbove = lists:reverse([Under| lists:reverse(Above)]),
    Score#score{above=NewAbove, below=[]}.

is_rubber_done(Winner, #game_state{is_NS_vulnerable=NSVulnerable, is_WE_vulnerable=WEVulnerable}=_State) ->
    is_vulnerable(Winner, NSVulnerable, WEVulnerable).


%%---------------------------------------------------------------------------------------------------------------------------
%% Function checking who won the game
%%---------------------------------------------------------------------------------------------------------------------------
who_won_game(#score{below=Below}) ->
    {{'NS', Ns}, {'WE', We}} = sum_points(Below),
    %% draw at this stage is not possible because in each deal only one team can score
    case Ns >= We of
        true -> 'NS';
        false -> 'WE'
    end.

%%---------------------------------------------------------------------------------------------------------------------------
%% Function checking who won the rubber
%%---------------------------------------------------------------------------------------------------------------------------
create_summary(#score{above=Above, below=[]}) ->
    create_summary(Above, #summary{}).

create_summary([], Summary) -> 
    set_winner_of_rubber(Summary);
create_summary([H|T], #summary{we_score=We, ns_score=Ns}=Summary) ->
    {{'NS', NsSum},{'WE', WeSum}} = sum_points(H),
    create_summary(T, Summary#summary{we_score=We+WeSum, ns_score=Ns+NsSum}).

%% =========
set_winner_of_rubber(#summary{we_score=S, ns_score=S}=Summary) ->
    Summary#summary{winner=draw};
set_winner_of_rubber(#summary{we_score=We, ns_score=Ns}=Summary) ->
    case Ns > We of
        true -> Summary#summary{winner='NS'};
        false -> Summary#summary{winner='WE'}
    end.
