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
%% API methods
%%-------------------------------------------------------------
%% -export([new_game/2, clear_scores/1, clear_last_game/1, process_game/2, set_players/2]).
-export([new_session/0, get_session/1, new_game/2, process_deal/3, remove_game/2, remove_deal/3]).
-export([set_player_name/3, switch_players/3]).


%%-------------------------------------------------------------------------------------------------------------------------
%%-------------------------------------------------------------
%% start / stop methods implementation
%%-------------------------------------------------------------
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?SERVER, stop).
%%-------------------------------------------------------------------------------------------------------------------------
%% API methods implementation
%%-------------------------------------------------------------
%%-------------------------------------------------------------
%% new_session
%%-------------------------------------------------------------
-spec new_session() -> Session::#bridge_session{}.

new_session() ->
	gen_server:call(?SERVER, {new_session}).

%%-------------------------------------------------------------
%% get_session
%%-------------------------------------------------------------
-spec get_session(SessionId::atom()) -> Session::#bridge_session{}.

get_session(SessionId) ->
	gen_server:call(?SERVER, {get_session, SessionId}).

%%-------------------------------------------------------------
%% new_game
%%-------------------------------------------------------------
-spec new_game(SessionId::atom(), GameType::atom()) -> State::#game_state{}.

new_game(SessionId, GameType) ->
    gen_server:call(?SERVER, {new_game, GameType, SessionId}).

%%-------------------------------------------------------------
%% process_deal
%%-------------------------------------------------------------
-spec process_deal(SessionId::atom(), GameType::atom(), Contract::#contract{}, Result::#result{}) -> State::#game_state{}.

process_deal(SessionId, GameType, Contract, Result) ->
    gen_server:call(?SERVER, {process_deal, SessionId, GameType, Contract, Result}).

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
-spec set_player_name(SessionId::atom(), Position::atom(), NewName::string()) -> Players::#players{}.

set_player_name(SessionId, Position, NewName) ->
	gen_server:call(?SERVER, {set_player_name, SessionId, Position, NewName}).
%%-------------------------------------------------------------
%% switch_players
%%-------------------------------------------------------------
-spec switch_players(SessionId::atom(), Position1::atom(), Position2::atom()) -> Players::#players{}. 

switch_players(SessionId, Position1, Position2) ->
    gen_server:call(?SERVER, {switch_players, SessionId, Position1, Position2}}.

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
handle_new_game(?TYPE_INTERNATIONAL, #session{stateInter=#game_state{game_id=GameNo}=Inter, players=ActPlayers, game_history=#history{inter=InterHistory}=History}=Session, Players) ->
    NewInter = #game_state{game_id=GameNo+1},
    _NewSession = case GameNo of
        0 -> % do not update history as it is first game
            Session#session{stateInter=NewInter, players=Players};
        _Other -> % move previous game_state to history
            NewInterHistory = [{GameNo, Inter, ActPlayers}|InterHistory],
            NewHistory = History#history{inter=NewInterHistory},
            % return updated session
            Session#session{stateInter=NewInter, players=Players, game_history=NewHistory}
    end;
handle_new_game(?TYPE_SPORT, _Session, _Players) ->
    {error, not_implemented};        
handle_new_game(?TYPE_IMP, _Session, _Players) ->
    {error, not_implemented}.        
