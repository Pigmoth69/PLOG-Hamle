:- use_module(library(clpfd)).
:- use_module(library(lists)).

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
	domain(Result, 0, 5),

	count(0, Result, #=, NumberWhites),
	
	restrictMovements(Values,NumberWhites,N,Result),
	blacksNotAdjacent(ResultBoard),

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

createTuples(0,[]).

createTuples(NumberBlacks,[L|ListTuples]):- NumberBlacks> 0,N1 is NumberBlacks - 1, length(L,1),createTuples(N1,ListTuples).

checkIfRight(Index-Value,N,[Dest]):-OriginalFloor is floor((Index-1) / N),
									Dest is (Index + Value),
									DestFloor is floor((Dest-1) / N),
									DestFloor =:= OriginalFloor.
							

checkIfRight(_,_,[]).

checkIfLeft(Index-Value,N,[Dest]):-OriginalFloor is floor((Index-1) / N),
									Dest is (Index - Value),
									DestFloor is floor((Dest-1)/N),
									DestFloor =:= OriginalFloor.
							

checkIfLeft(_,_,[]).

checkIfUp(Index-Value,N,[Dest]):-Dest is (Index - Value*N),
									Dest >= 1.
									
checkIfUp(_,_,[]).

checkIfDown(Index-Value,N,[Dest]):-Dest is (Index + Value*N),
									Dest =< (N*N).
									
checkIfDown(_,_,[]).

tablingTuples([],[],_).

tablingTuples([Tuple|ListTuples],[Piece|ListBlacks],N):-
									checkIfRight(Piece,N,Dest1),
									checkIfLeft(Piece,N,Dest2),
									checkIfUp(Piece,N,Dest3),
									checkIfDown(Piece,N,Dest4),
									table([[Tuple]],[Dest1,Dest2,Dest3,Dest4]),
									tablingTuples(ListTuples,ListBlacks,N).


putPiecesInBoard([],[],_):-print('aqui').									
									
putPiecesInBoard([Tuple|ListTuples],[_-Value | ListBlacks],Result):-element(Tuple,Result,Value),
																	putPiecesInBoard(ListTuples, ListBlacks ,Result).



restrictMovements(Board,NumberWhites,N,Result):-
				NumberBlacks is (N*N)-NumberWhites,
				length(ListTuples,NumberBlacks),
				findall(Index-Value,(nth1(Index,Board,Value),Value>0),ListBlacks),
				tablingTuples(ListTuples,ListBlacks,N),
				all_different(ListTuples),
				putPiecesInBoard(ListTuples,ListBlacks,Result).




blacksMovement(_, _, _, 0).
blacksMovement(Values, Result, N, Position):-
	element(Position, Values, Value), Value > 0, moveBlack(Result, Position, Value, N),write('Chega aqui'),
	NextPosition is Position - 1,
	blacksMovement(Values, Result, N, NextPosition).
blacksMovement(Values, Result, N, Position):-
	NextPosition is Position - 1,
	blacksMovement(Values, Result, N, NextPosition).

moveBlack(Result, Position, Value, N):-
	(NewPosition #= Position - N*Value),
	element(NewPosition, Result, 1),
	write(NewPosition),nl.
moveBlack(Result, Position, Value, N):-
	(NewPosition #= Position + N*Value),
	element(NewPosition, Result, 2),
	write(NewPosition),nl.
moveBlack(Result, Position, Value, _):-
	(NewPosition #= Position - Value),
	element(NewPosition, Result, 3),
	write(NewPosition),nl.
moveBlack(Result, Position, Value, _):-
	(NewPosition #= Position + Value),
	element(NewPosition, Result, 4),
	write(NewPosition),nl.

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