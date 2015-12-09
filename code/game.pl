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
	length(Values, Length),
	length(Result, Length),
	domain(Result, 0, 4),

	count(0, Result, #=, NumberWhites),
	blacksNotAdjacent(Result, N),                          

	print(Result),nl,
	labeling([], Result).

blacksNotAdjacent(Result, N):-
	setUpBoard(Result, N, ResultBoard),
	transpose(ResultBoard, TResultBoard),
	checkAdjacentCells(TResultBoard),
	checkAdjacentCells(ResultBoard).

checkAdjacentCells([]).
checkAdjacentCells([Head | Tail]):-
	checkAdjacentCellsRow(Head),
	checkAdjacentCells(Tail).

checkAdjacentCellsRow([_ | []]).
checkAdjacentCellsRow([First, Second | Tail]):-
	First #>0, Second #= 0,
	checkAdjacentCellsRow([Second | Tail]).
checkAdjacentCellsRow([_ | Tail]):-
	checkAdjacentCellsRow(Tail).
