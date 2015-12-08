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

	functionTest(Values, 1, N, Result),
	checkBlacksAdjacentCells(Result, N),
	print(Result),nl,
	labeling([], Result).


functionTest([], _, _, _).
functionTest([Value | Tail], Position, N, Result):-
	Value #\= 0, 
	moveBlackCell(Value, Position, N, Result),
	/*element(N, Result, Value),
	Value #> 0,*/
	NextPosition #= Position + 1,
	functionTest(Tail, NextPosition, N, Result).
functionTest([_ | Tail], Position, N, Result):-
	NextPosition #= Position + 1,
	functionTest(Tail, NextPosition, N, Result).


%move up
moveBlackCell(Value, Position, N, Result):-
	BlackPosition #= Position - N*Value,
	element(BlackPosition, Result, 1).	

%moved down
moveBlackCell(Value, Position, N, Result):-
	BlackPosition #= Position - N*Value,
	element(BlackPosition, Result, 2).

%moved right
moveBlackCell(Value, Position, N, Result):-
	BlackPosition #= Position + Value,
	element(BlackPosition, Result, 3).

%moved left
moveBlackCell(Value, Position, N, Result):-
	BlackPosition #= Position - Value,
	element(BlackPosition, Result, 4).

checkBlacksAdjacentCells(Result, N, Position):-
	N*N #< Position.
checkBlacksAdjacentCells(Result, N, Position):-
	element(Position, Result, Value),
	Value #> 0,
	Up #= Position - N,
	Down #= Position + N,
	Right #= Position + 1,
	Left #= Position - 1,
	element(Up, Result, UpValue),
	element(Down, Result, DownValue),
	element(Right, Result, RightValue),
	element(Left, Result, LeftValue),
	UpValue #= 0, DownValue #= 0, RightValue #= 0, LeftValue #= 0,
	NextPosition #= Position + 1,
	checkBlacksAdjacentCells(Result, N, NextPosition).



sameRowAfterMovingRight(OriginalPosition, Position, N):-
	Position mod N #= 1.
sameRowAfterMovingRight(OriginalPosition, Position, N):-
	OriginalPosition mod N #< Position mod N.

sameRowAfterMovingLeft(OriginalPosition, Position, N):-
	OriginalPosition mod N #= 1.
sameRowAfterMovingLeft(OriginalPosition, Position, N):-
	Position mod N #< OriginalPosition mod N.

