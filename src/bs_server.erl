-module(bs_server).

-vsn(0.1).

-behaviour(gen_server).

-include("bs_data.hrl").

%%-------------------------------------------------------------------------------------------------------------------------
%%-------------------------------------------------------------
%% state definition 
%%-------------------------------------------------------------
% XXX temporary state - finally sessions shall be moved to database
-record(state, {sessions=[]}).
%%-------------------------------------------------------------------------------------------------------------------------
%%-------------------------------------------------------------
%% types declaration
%%-------------------------------------------------------------

%%-------------------------------------------------------------------------------------------------------------------------
%%-------------------------------------------------------------
%% defines
%%-------------------------------------------------------------
-define(SERVER, ?MODULE).

% game scoring types
-define(TYPE_INTERNATIONAL, rubber).
-define(TYPE_SPORT, sport).
-define(TYPE_IMP, imp).
-define(KNOWN_GAME_TYPES, [rubber, sport, imp]).

%%-------------------------------------------------------------------------------------------------------------------------
%%-------------------------------------------------------------
%% start / stop methods
%%-------------------------------------------------------------
-export([start_link/0, stop/0]).
%%-------------------------------------------------------------------------------------------------------------------------
%%-------------------------------------------------------------
%% gen_server callbacks
%%-------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%-------------------------------------------------------------------------------------------------------------------------
%%-------------------------------------------------------------
%% API methods
%%-------------------------------------------------------------
%% -export([new_game/2, clear_scores/1, clear_last_game/1, process_game/2, set_players/2]).
-export([new_session/0, get_session/1, new_game/2, process_deal/4, remove_game/2, remove_deal/3]).
-export([set_player_name/3, switch_players/3]).


%%-------------------------------------------------------------------------------------------------------------------------
%%-------------------------------------------------------------
%% start / stop methods implementation
%%-------------------------------------------------------------
start_link() ->
	log("Starting server"),
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
	log("Stopping server"),
	gen_server:cast(?SERVER, stop).
%%-------------------------------------------------------------------------------------------------------------------------
%% API methods implementation
%%-------------------------------------------------------------
%%-------------------------------------------------------------
%% new_session
%%-------------------------------------------------------------
-spec new_session() -> Session::#bridge_session{}.

new_session() ->
	log("Calling for new session"),
	gen_server:call(?SERVER, {new_session}).

%%-------------------------------------------------------------
%% get_session
%%-------------------------------------------------------------
-spec get_session(SessionId::atom()) -> Session::#bridge_session{} | {badarg, non_atom_id}.

get_session(SessionId) when is_atom(SessionId) ->
	gen_server:call(?SERVER, {get_session, SessionId});
get_session(_) ->
    {badarg, non_atom_id}.

%%-------------------------------------------------------------
%% new_game
%%-------------------------------------------------------------
-spec new_game(SessionId::atom(), GameType::atom()) -> Session::#bridge_session{} |
                                                        {badarg, non_atom_id} | 
                                                        {badarg, non_atom_game_type} | 
                                                        {badarg, unknown_game_type} | 
                                                        {badarg, unknown_session}.

new_game(SessionId, _GameType) when not is_atom(SessionId) ->
    {badarg, non_atom_id};
new_game(_SessionId, GameType) when not is_atom(GameType) ->
    {badarg, non_atom_game_type};
new_game(SessionId, GameType) ->
    %log("Starting new game -> API invoked"),
    case lists:member(GameType, ?KNOWN_GAME_TYPES) of
        false -> {badarg, unknown_game_type};
        true -> gen_server:call(?SERVER, {new_game, SessionId, GameType})
    end.

%%-------------------------------------------------------------
%% process_deal
%%-------------------------------------------------------------
-spec process_deal(SessionId::atom(), GameType::atom(), Contract::#contract{}, Result::#result{}) -> State::#game_state{}.

process_deal(SessionId, GameType, Contract, Result) ->
    GameState = gen_server:call(?SERVER, {process_deal, SessionId, GameType, Contract, Result}),
    case GameState#game_state.status of
        unfinished ->
            ok;
        _ ->
            new_game(SessionId, GameType)
    end,
    GameState.

%%-------------------------------------------------------------
%% remove_game
%%-------------------------------------------------------------
-spec remove_game(SessionId::atom(), GameId::atom()) -> ok | error.

remove_game(SessionId, GameId) ->
	gen_server:call(?SERVER, {remove_game, SessionId, GameId}).
%%-------------------------------------------------------------
%% remove_deal
%%-------------------------------------------------------------
-spec remove_deal(SessionId::atom(), GameId::atom(), RoundNo::integer()) -> Result::#game_state{}.

remove_deal(SessionId, GameId, RoundNo) ->
	gen_server:call(?SERVER, {remove_deal, SessionId, GameId, RoundNo}).
%%-------------------------------------------------------------
%% set_player_name
%%		position: nort | south | west | east
%%-------------------------------------------------------------
-spec set_player_name(SessionId::atom(), Position::atom(), NewName::string()) -> Players::[#player{}]. 

set_player_name(SessionId, Position, NewName) ->
	gen_server:call(?SERVER, {set_player_name, SessionId, Position, NewName}).
%%-------------------------------------------------------------
%% switch_players
%%-------------------------------------------------------------
-spec switch_players(SessionId::atom(), Position1::atom(), Position2::atom()) -> Players::list(). 

switch_players(SessionId, Position1, Position2) ->
    gen_server:call(?SERVER, {switch_players, SessionId, Position1, Position2}).

%%-------------------------------------------------------------------------------------------------------------------------
%%-------------------------------------------------------------
%% gen_server callbacks implementation
%%-------------------------------------------------------------
%%-------------------------------------------------------------
%% init
%%-------------------------------------------------------------
init(_Args) ->
    State = #state{},
    {ok, State}.

%%-------------------------------------------------------------
%% handle_call
%%-------------------------------------------------------------
handle_call({new_session}, _From, State)->
	log("Handling call for new session. Current state is ~p", [State]),
	Session = create_session(),
	log("Session created"),
	NewState = save_session(Session, State), 
	log("State updated. New state is ~p", [NewState]),
	{reply, Session, NewState};

handle_call({get_session, SessionId}, _From, State) ->
	Session = case get_session(SessionId, State) of
        {badarg, unknown_session} -> create_session(SessionId);
        ASession -> ASession
    end,
	NewState = save_session(Session, State),
	{reply, Session, NewState}; 

handle_call({new_game, SessionId, GameType}, _From, State)->
    log("Starting new game -> gen_server callback"),
    case get_session(SessionId, State) of
        {badarg, unknown_session}=Reply -> 
            {reply, Reply, State};
        Session ->
            NewSession = handle_new_game(GameType, Session),
            NewState = save_session(NewSession, State),
            {reply, NewSession, NewState}
    end;

handle_call({process_deal, SessionId, GameType, Contract, Result}, _From, State)->
    Session = get_session(SessionId, State),
    NewGameState = handle_process_deal(Session, GameType, Contract, Result),
    NewSession = Session#bridge_session{games_states=set_current_game(NewGameState, Session#bridge_session.games_states)},
    NewState = save_session(NewSession, State),
    {reply, NewGameState, NewState};

handle_call({remove_game, SessionId, GameId}, _From, State) ->
	Session = get_session(SessionId, State),
	NewSession = handle_remove_game(Session, GameId),
	NewState = save_session(NewSession, State),
	{reply, ok, NewState};

handle_call({remove_deal, SessionId, GameId, RoundNo}, _From, State) ->
	Session = get_session(SessionId, State),
	{GameState, NewSession} = handle_remove_deal(Session, GameId, RoundNo),
	NewState = save_session(NewSession, State),
	{reply, GameState, NewState};

handle_call({set_player_name, SessionId, Position, Name}, _From, State) ->
	Session = get_session(SessionId, State),
	#bridge_session{players=Players}=NewSession = handle_player_name_change(Session, Position, Name),
	NewState = save_session(NewSession, State),
	{reply, Players, NewState};

handle_call({switch_players, SessionId, Position1, Position2}, _From, State) ->
	Session = get_session(SessionId, State),
	#bridge_session{players=Players}=NewSession = handle_players_switch(Session, Position1, Position2),
	NewState = save_session(NewSession, State),
	{reply, Players, NewState}.


%%-------------------------------------------------------------
%% handle_cast
%%-------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%-------------------------------------------------------------
%% handle_info
%%-------------------------------------------------------------
handle_info(_Msg, State) ->
    {noreply, State}.

%%-------------------------------------------------------------
%% terminate
%%-------------------------------------------------------------
terminate(shutdown, _State) ->
    ok;
terminate(_Reason, _State) ->
    error.

%%-------------------------------------------------------------
%% code_change
%%-------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%-------------------------------------------------------------------------------------------------------------------------
%%-------------------------------------------------------------
%% Private functions implementation
%%-------------------------------------------------------------

%% ==========================
%% create_session
%% ==========================
create_session() ->
	Id = generate_id(),
	create_session(Id).

create_session(Id) when is_atom(Id) ->
	Players = create_players(),
    Games = [{T, create_new_game(T)} || T <- [rubber, sport, imp]],
	#bridge_session{id=Id, games_states=Games, players=Players}.

%% ===
generate_id() ->
    list_to_atom(lists:flatten(io_lib:format("~p_~p_~p", tuple_to_list(now())))).

%% ===
create_players() ->
	North = #player{id=player_one, position=north, name="Player N"},
	South = #player{id=player_two, position=south, name="Player S"},
	West = #player{id=player_three, position=west, name="Player W"},
	East = #player{id=player_four, position=east, name="Player E"},
	[North, South, West, East].

%%---------------------------
%%  get_session from db (temporarly from state)
%%---------------------------
get_session(SessionId, #state{sessions=Sessions}) ->
    lists:foldl(fun(X, Acc) -> case X#bridge_session.id of
                        SessionId -> X;
                        _ -> Acc
                    end 
                end, {badarg, unknown_session}, Sessions).

%% ====
find_session(SessionId, [#bridge_session{id=SessionId}=Session|_T]) ->
	Session;
find_session(SessionId, [_H|T]) ->
	find_session(SessionId, T);
find_session(_SessionId, []) ->
	{error, no_session}.
%% ===


%%---------------------------
%%  save_session into db
%%---------------------------
save_session(#bridge_session{id=Id}=Session, #state{sessions=Sessions}=State) ->
    NewSessions = case find_session(Id, Sessions) of
        {error, no_session} -> [Session|Sessions];
        _ -> lists:map(fun(#bridge_session{id=Sid}=X) -> 
                case Sid of
                    Id -> Session;
                    _ -> X 
                end end, Sessions)
    end,
    State#state{sessions=NewSessions}.

%%---------------------------
%% handle_new_game
%%---------------------------
handle_new_game(GameType, #bridge_session{games_states=GamesStates, players=Players, history=History}=Session) ->
    log("Starting new game -> handler method"),
	% move current game to history
    CurrentGame = lists:foldl(fun({T, GS}, AccIn) -> case T of 
                    GameType -> GS;
                    _ -> AccIn  
                end end, {error, unknown_game_type, GameType}, GamesStates), 
	HistoryEntry = {CurrentGame, Players},
	NewHistory = insert_into_history(GameType, HistoryEntry, History),
	% create new game
    NewGamesStates = lists:map(fun({T, GS}) -> case T of 
                    GameType -> {T, create_new_game(T)};
                    _ -> {T, GS}
                end end, GamesStates),
	Session#bridge_session{games_states=NewGamesStates, history=NewHistory}.
	
%% ===
get_current_game(GameType, [{GameType, CurrentGame}|_T]) ->
	CurrentGame;
get_current_game(GameType, [{_,_}|T]) ->
	get_current_game(GameType, T).
%% ===
set_current_game(GameState, GamesStates) ->
    set_current_game(GameState, GamesStates, []).

set_current_game(_Game, [], NewStates) ->
    NewStates;
set_current_game(#game_state{game_type=Type}=Game, [{Type, _Prev}|T], NewStates) ->
    set_current_game(Game, T, [{Type, Game}|NewStates]);
set_current_game(Game, [H|T], NewStates) ->
    set_current_game(Game, T, [H|NewStates]).

%% ===
create_new_game(GameType) ->
	#game_state{game_id=generate_id(), game_type=GameType}.
%% ===
insert_into_history(rubber, Entry, #history{rubber=Old}=History) ->
	History#history{rubber=[Entry|Old]};
insert_into_history(sport, Entry, #history{sport=Old}=History) ->
	History#history{sport=[Entry|Old]};
insert_into_history(imp, Entry, #history{imp=Old}=History) ->
	History#history{imp=[Entry|Old]}.
%% ===

%% =============================
%% handle_process_deal
%% =============================
handle_process_deal(Session, GameType, Contract, Result) ->
	GameState = get_current_game(GameType, Session#bridge_session.games_states),
	handle_process_deal(GameState, Contract, Result).

handle_process_deal(#game_state{game_type=rubber}=GameState, Contract,#result{taken=Taken}=_Result) ->
	bs_rubber_score:process(Contract, Taken, GameState);
handle_process_deal(_GameState, _Contract, _Result) ->
	{error, unknown_game_type}.
		
%% =============================
%% handle_remove_game
%% =============================
handle_remove_game(Session, GameId) ->
	{error, not_yet_implemented}.
%% =============================
%% handle_remove_deal
%% =============================
handle_remove_deal(Session, GameId, RoundNo) ->
	{error, not_implemented_yet}.
%% =============================
%% handle_player_name_changed
%% =============================
handle_player_name_change(#bridge_session{players=Players}=Session, Position, Name) ->
	NewPlayers = lists:map(fun(#player{position=Pos}=Player) -> case Pos == Position of 
														true -> Player#player{name=Name}; 
														false -> Player end end, Players),
	Session#bridge_session{players=NewPlayers}.

%% =============================
%% handle_players_switch
%% =============================
handle_players_switch(#bridge_session{players=Players}=Session, Position1, Position2) ->
	NewPlayers = do_players_switch(Players, Position1, Position2),
	Session#bridge_session{players=NewPlayers}.

do_players_switch(Players, Pos1, Pos2) ->
	do_players_switch(Players, Pos1, Pos2, []).

do_players_switch([], _, _, NewPlayers) ->
	lists:reverse(NewPlayers);
do_players_switch([#player{position=Pos}=Switched|T], Pos, NewPos, NewPlayers) ->
	do_players_switch(T, Pos, NewPos, [Switched#player{position=NewPos}|NewPlayers]);
do_players_switch([#player{position=Pos}=Switched|T], NewPos, Pos, NewPlayers) ->
	do_players_switch(T, NewPos, Pos, [Switched#player{position=NewPos}|NewPlayers]);
do_players_switch([H|T], Pos1, Pos2, NewPlayers) ->
	do_players_switch(T, Pos1, Pos2, [H|NewPlayers]).


%% ============================================================================================
%% Log producing finctions
%% ============================================================================================
log(Msg) ->
	log(Msg, []).

log(Format, Args) ->
	io:format("~p| "++Format++"~n", [self()|Args]).
