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
%--------------SOLUTION--------------%
%------------------------------------%
solution(Values, NumberWhites, N, Result):-
	length(Values, Length),
	length(Result, Length),
	setUpBoard(Values, N, ValuesBoard),
	setUpBoard(Result, N, ResultBoard),
	domain(Result, 0, 4),

	count(0, Result, #=, NumberWhites),
	blacksNotAdjacent(ResultBoard),
	blacksMovement(Values, Result, N, Length),

	print(Result),nl,
	labeling([], Result).



%------------------------------------%
%------------RESTRICTIONS------------%
%------------------------------------%


%*************************************%
%*****BLACKS ADJACENCY CONSTRAIN******%
%*************************************%
blacksNotAdjacent(ResultBoard):-
	transpose(ResultBoard, TResultBoard),
	checkAdjacentCells(TResultBoard),
	checkAdjacentCells(ResultBoard).

checkAdjacentCells([]).
checkAdjacentCells([Head | Tail]):-
	checkAdjacentCellsLine(Head),
	checkAdjacentCells(Tail).


checkAdjacentCellsLine([_ | []]).
checkAdjacentCellsLine([First, Second | Tail]):-
	First #>0 #=> Second #= 0,
	checkAdjacentCellsLine([Second | Tail]).
checkAdjacentCellsLine([_ | Tail]):-
	checkAdjacentCellsLine(Tail).


%***************************************%
%*****BLACKS MOVEMENTS CONSTRAIN********%
%***************************************%
blacksMovement(_, _, _, 0).
blacksMovement(Values, Result, N, Position):-
	element(Position, Values, Value), Value #> 0 #=> moveBlack(Result, Position, Value, N),
	NextPosition is Position - 1,
	blacksMovement(Values, Result, N, NextPosition).
blacksMovement(Values, Result, N, Position):-
	NextPosition is Position - 1,
	blacksMovement(Values, Result, N, NextPosition).

moveBlack(Result, Position, Value, N):-
	NewPosition #= Position - N*Value,
	element(NewPosition, Result, 1).
moveBlack(Result, Position, Value, N):-
	NewPosition #= Position + N*Value,
	element(NewPosition, Result, 2).
moveBlack(Result, Position, Value, N):-
	NewPosition #= Position - Value,
	element(NewPosition, Result, 3).
moveBlack(Result, Position, Value, N):-
	NewPosition #= Position + Value,
	element(NewPosition, Result, 4).

/*
blacksMovement(ResultBoard, ValuesBoard):-
	checkHorizontalMovement(ValuesBoard, ResultBoard).
blacksMovement(ResultBoard, ValuesBoard):-
	transpose(ResultBoard, TResultBoard),
	transpose(ValuesBoard, TValuesBoard),
	checkVerticalMovement(TValuesBoard, TResultBoard).

checkHorizontalMovement([], _).
checkHorizontalMovement([Head | Tail], [RHead | RTail]):-
	is_list(Head),
	checkHorizontalMovement(Head, RHead),
	checkHorizontalMovementLine(Tail, RTail).
%esquerda
checkHorizontalMovementLine(Values, Result):-
	element(Position, Values, Value),
	Value #> 0,
	NewPosition #= Position - Value,
	element(NewPosition, Result, 3).
%direita
checkHorizontalMovementLine(Values, Result):-
	element(Position, Values, Value),
	Value #> 0,
	NewPosition #= Position + Value,
	element(NewPosition, Result, 4).

checkVerticalMovement([], _).
checkVerticalMovement([Head | Tail], [RHead | RTail]):-
	is_list(Head),
	checkVerticalMovementLine(Head, RHead),
	checkVerticalMovement(Tail, RTail).
%cima
checkVerticalMovementLine(Values, Result):-
	element(Position, Values, Value),
	Value #> 0,
	NewPosition #= Position - Value,
	element(NewPosition, Result, 1).
%baixo
checkVerticalMovementLine(Values, Result):-
	element(Position, Values, Value),
	Value #> 0,
	NewPosition #= Position + Value,
	element(NewPosition, Result, 2).
	*/