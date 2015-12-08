:- use_module(library(clpfd)).

defaultGame:- 
	defaultInfoList(Info),
	setUpBoard(Info, 6, Board),
	displayBoard(Board),
	getEnter,
	solution(Info,26, 6, 36, Result),
	setUpBoard(Result, 6, FinalBoard),
	displayBoard(FinalBoard).

newGame:- 
	getBoardSize(Size),
	generateBoard(Size,Size,Board),
	displayBoard(Board).


%-------------------------------------%
%-----------MOVE VALIDATION-----------%
%-------------------------------------%

getBlackCells([], []).
getBlackCells([0 | Tail], Blacklist):-
	getBlackCells(Tail, Blacklist).
getBlackCells([Head | Tail], [Head | Blacklist]):-
	getBlackCells(Tail, Blacklist).





%------------------------------------%
%------------RESTRICTIONS------------%
%------------------------------------%

solution(Values, NumberWhites, N, Result):-
	length(Values, L),
	length(Result, L),
	domain(Result, 0, 4),
	count(0, Result, #=, NumberWhites),


	element(Position, Result, Value)
	moveBlackCell(Position, N, Value, Values),
	%checkBlackPosition(Position, N, Result),

	/*print(Result),nl,*/
	labeling([], Result).

%move up
moveBlackCell(Position, N, Value, Values):-
	OriginalPosition #= Position + N*Value,
	element(OriginalPosition, Values, Value).

%move down
moveBlackCell(Position, N, Value, Values):-
	OriginalPosition #= Position - N*Value,
	element(OriginalPosition, Values, Value).

%move right
moveBlackCell(Position, _, Value, Values):-
	OriginalPosition #= Position - Value,
	%sameRowAfterMovingRight(OriginalPosition, Position, N),
	element(OriginalPosition, Values, Value).

%move left
moveBlackCell(Position, _, Value, Values):-
	OriginalPosition #= Position + Value,
	%sameRowAfterMovingLeft(OriginalPosition, Position, N),
	element(OriginalPosition, Values, Value).

sameRowAfterMovingRight(OriginalPosition, Position, N):-
	Position mod N #= 1.
sameRowAfterMovingRight(OriginalPosition, Position, N):-
	OriginalPosition mod N #< Position mod N.

sameRowAfterMovingLeft(OriginalPosition, Position, N):-
	OriginalPosition mod N #= 1.
sameRowAfterMovingLeft(OriginalPosition, Position, N):-
	Position mod N #< OriginalPosition mod N.