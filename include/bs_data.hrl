
%%---------------------------------------------------------------------
%% constants used in modules
%%---------------------------------------------------------------------
-define(POINTS_FOR_CLUBS_TRICK, 20).
-define(POINTS_FOR_DIMONDS_TRICK, 20).
-define(POINTS_FOR_HEARTS_TRICK, 30).
-define(POINTS_FOR_SPADES_TRICK, 30).
-define(POINTS_FOR_NOTRUMPH_TRICK, 30).
-define(POINTS_FOR_FIRST_NOTRUMPH_TRICK, 40).


%%---------------------------------------------------------------------
%% Data Type: contract
%% where:
%%	owner:	   'NS'|'WE'	A pair who declared this contract	
%%	color:	   'N'|'S'|'H'|'D'|'C' (Notrump, Spades, Hearts, Dimonds, Clubs)
%% 	level:	   1|2|3|4|5|6|7
%%	doubled:   boolean() (default is false)
%%	redoubled: boolean() (default is false)
%%---------------------------------------------------------------------
-record(contract, {owner, color, level, doubled=false, redoubled=false}).

%%---------------------------------------------------------------------
%% Data Type: result
%% where:
%%	taken:	0..13 	A number of taken odds 
%%	milton:	A number of Honor Points summed from both players
%%---------------------------------------------------------------------
-record(result, {taken, miltons}).

%%---------------------------------------------------------------------
%% TODO Move is_closed param to game_state record
%% Data Type: score
%% where:
%%	above: [[score_entry]]	A list of lists of entries to be put above the line in rubber scoring scheme
%%	below: [score_entry]	A list of entries to be put below the line in rubber scoring scheme
%%	is_closed: boolean()	A flag indicating if rubber is finished or not
%%---------------------------------------------------------------------
-record(score, {above=[[]], below=[], is_closed=false}).

%%---------------------------------------------------------------------
%% Data Type: score_entry (used in rubber scoring scheme)
%% where:
%%	count:	A number of this entry (A number of deal in rubber)
%%	'WE':	[{points, comment}]	A list of tuples containig points earned by 'WE' 
%%			in this deal and comments explaining what are the points for.
%%	'NS':	[{points, comment}]	A list of tuples containig points earned by 'NS' 
%%			in this deal and comments explaining what are the points for.
%%---------------------------------------------------------------------
-record(score_entry, {count, 'WE'=[], 'NS'=[]}).

%%---------------------------------------------------------------------
%% Data Type: score_entry_imp (used in milton scoring scheme)
%% where:
%%	count: 	A number of this entry
%%	first:	A number of points earned by first player in this round
%%	second:	A number of points earned by second player in this round
%%	third:	A number of points earned by third player in this round
%%	fourth:	A number of points earned by fourth player in this round
%%
%%	Players are signed differently than 'N' 'S' 'W' 'E' because they can shuffle
%%	after each rubber
%%---------------------------------------------------------------------
-record(score_entry_imp, {count, first, second, third, fourth}). 

%%---------------------------------------------------------------------
%% Data Type: summary   (used when game is finished)
%% where:
%%  winner:     'WE' | 'NS'
%%  we_score:   integer()
%%  ns_score:   integer()
%%---------------------------------------------------------------------
-record(summary, {winner, we_score, ns_score}).

%%---------------------------------------------------------------------
%% Data Type: game_state
%% where:
%%  game_id:    atom()
%%  game_type:  rubber | sport | imp
%%  round_no:   integer()
%%  score:      #score()
%%  is_WE_vulnerable:   boolean()
%%  is_NS_vulnerable:   boolean()
%%  status:     unfinished | #summary()
%%---------------------------------------------------------------------
-record(game_state, {game_id, game_type, round_no=1, score=#score{}, is_WE_vulnerable=false, is_NS_vulnerable=false, status=unfinished}).

%%---------------------------------------------------------------------
%% Data Type: player
%% where:
%%  id:         An identifier unique through the bridge session, helps to keep track of player name or position changes
%%  position:   north | east | south | west
%%  name:       Name of player.
%%---------------------------------------------------------------------
-record(player, {id, position, name}).

%%---------------------------------------------------------------------
%% Data Type: history
%% where:
%%	rubber:   A list of tuples containing #game_state and players for rubbernational notation
%%	sport :  A list of tuples containing #game_state and players for sport notation
%%	imp :    A list of tuples containing #game_state and players for imp notation
%%---------------------------------------------------------------------
-record(history, {rubber=[] , sport=[], imp=[]}).

%%---------------------------------------------------------------------
%% Data Type: bridge_session
%% where:
%%  id:             atom() 
%%  games_states:   list() - list of #game_state() !!! Will be changed !!!
%%  players:        list() - list of #player()
%%  history:        #history()
%% TODO:
%%  - Change games_states into list of records (not tuples containing redundant atom)
%%---------------------------------------------------------------------
-record(bridge_session, {id, games_states=[{rubber, #game_state{game_type=rubber}}, {sport, #game_state{game_type=sport}}, {imp, #game_state{game_type=imp}}], players=[], history=#history{}}).
