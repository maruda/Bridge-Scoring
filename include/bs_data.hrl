
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
%%---------------------------------------------------------------------
-record(game_state, {game_type, score=#score{}, is_WE_vulnerable=false, is_NS_vulnerable=false, round_no=0, game_id}).

%%---------------------------------------------------------------------
%%---------------------------------------------------------------------
-record(player, {id, position, name}).

%%---------------------------------------------------------------------
%%---------------------------------------------------------------------
% -record(players, {north=#player{}, south=#player{}, west=#player{}, east=#player{}}).

%%---------------------------------------------------------------------
%% Data Type: history
%% where:
%%	inter: A list of tuples containing #game_state and #players for international notation
%%	sport : A list of tuples containing #game_state and #players for sport notation
%%	imp : A list of tuples containing #game_state and #players for imp notation
%%---------------------------------------------------------------------
-record(history, {inter=[] , sport=[], imp=[]}).

%%---------------------------------------------------------------------
%%---------------------------------------------------------------------
-record(bridge_session, {id, games_states=[{inter, #game_state{game_type=inter}}, {sport, #game_state{game_type=sport}}, {imp, #game_state{game_type=imp}}], players=[], history=#history{}}).
