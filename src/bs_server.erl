-module(bs_server).

-vsn(0.1).

-behaviour(gen_server).

-include("bs_data.hrl").

%%-------------------------------------------------------------------------------------------------------------------------
%%-------------------------------------------------------------
%% state definition 
%%-------------------------------------------------------------
-record(players_mapping, {'N', 'E', 'S', 'W'}).
-record(history, {inter=[], sport=[], imp=[]}).
-record(session, {id, stateInter=#state{}, stateSport=#state{}, stateIMP=#state{}, players=#players_mapping{}, game_history=#history{}}).
-record(app_state, {}).

%%-------------------------------------------------------------------------------------------------------------------------
%%-------------------------------------------------------------
%% types declaration
%%-------------------------------------------------------------
-type game_data() :: {Contract::#contract{}, Result::#result{}}.
-type players_mapping() :: #players_mapping{}.

%%-------------------------------------------------------------------------------------------------------------------------
%%-------------------------------------------------------------
%% defines
%%-------------------------------------------------------------
-define(SERVER, ?MODULE).

% game scoring types
-define(TYPE_INTERNATIONAL, inter).
-define(TYPE_SPORT, sport).
-define(TYPE_IMP, imp).

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
%% API callbacks
%%-------------------------------------------------------------
-export([new_game/2, clear_scores/1, clear_last_game/1, process_game/2, set_players/2]).


%%-------------------------------------------------------------------------------------------------------------------------
%%-------------------------------------------------------------
%% start / stop methods implementation
%%-------------------------------------------------------------
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?SERVER, stop).
%%-------------------------------------------------------------------------------------------------------------------------
%% API callbacks implementation
%%-------------------------------------------------------------
%%-------------------------------------------------------------
%% new_game
%%-------------------------------------------------------------
-spec new_game(SessionId::atom(), {GameType::atom(), Players::#players_mapping{}}) -> State::#state{}.

new_game(SessionId, {GameType, #players_mapping{}=Players}) ->
    gen_server:call(?SERVER, {new_game, GameType, SessionId, Players}).

%%-------------------------------------------------------------
%% clear_scores
%%-------------------------------------------------------------
-spec clear_scores(SessionId::atom()) -> State::#state{}.

clear_scores(SessionId) ->
    #state{}.

%%-------------------------------------------------------------
%% clear_last_game
%%-------------------------------------------------------------
-spec clear_last_game(SessionId::atom()) -> State::#state{}.

clear_last_game(SessionId) ->
    #state{}.

%%-------------------------------------------------------------
%% process_game
%%-------------------------------------------------------------
-spec process_game(SessionId::atom(), Data::game_data()) -> State::#state{}.

process_game(SessionId, Data) ->
    #state{}.

%%-------------------------------------------------------------
%% set_players
%%-------------------------------------------------------------
-spec set_players(SessionId::atom(), Players::#players_mapping{}) -> State::#state{}.

set_players(SessionIs, Players) ->
    #state{}.

%%-------------------------------------------------------------------------------------------------------------------------
%%-------------------------------------------------------------
%% gen_server callbacks implementation
%%-------------------------------------------------------------
%%-------------------------------------------------------------
%% init
%%-------------------------------------------------------------
init(_Args) ->
    State = #app_state{},
    {ok, State}.

%%-------------------------------------------------------------
%% handle_call
%%-------------------------------------------------------------
handle_call({new_game, GameType, SessionId, Players}, _From, State)->
    Session = get_session(SessionId),
    NewSession = handle_new_game(GameType, Players, Session),
    save_session(SessionId, NewSession),
    {reply, NewSession}.

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
    ok.

%%-------------------------------------------------------------
%% code_change
%%-------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%-------------------------------------------------------------------------------------------------------------------------
%%-------------------------------------------------------------
%% Private functions implementation
%%-------------------------------------------------------------

%%---------------------------
%%  get_session from db
%%---------------------------
get_session(SessionId) ->
    % TODO
    {error, not_implemented}.

%%---------------------------
%%  save_session into db
%%---------------------------
save_session(SessionId, Session) ->
    {error, not_implemented}.

%%---------------------------
%% handle_new_game
%%---------------------------
handle_new_game(?TYPE_INTERNATIONAL, #session{stateInter=#state{gameNo=GameNo}=Inter, players=ActPlayers, game_history=#history{inter=InterHistory}=History}=Session, Players) ->
    NewInter = #state{gameNo=GameNo+1},
    _NewSession = case GameNo of
        0 -> % do not update history as it is first game
            Session#session{stateInter=NewInter, players=Players};
        _Other -> % move previous state to history
            NewInterHistory = [{GameNo, Inter, ActPlayers}|InterHistory],
            NewHistory = History#history{inter=NewInterHistory},
            % return updated session
            Session#session{stateInter=NewInter, players=Players, game_history=NewHistory}
    end;
handle_new_game(?TYPE_SPORT, _Session, _Players) ->
    {error, not_implemented};        
handle_new_game(?TYPE_IMP, _Session, _Players) ->
    {error, not_implemented}.        
