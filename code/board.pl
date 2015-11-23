defaultBoard(
	[
		[' ', '3', ' ', ' ', ' ', '2'],
		[' ', ' ', '3', ' ', '4', ' '],
		[' ', '1', ' ', ' ', ' ', ' '],
		['5', ' ', ' ', '2', ' ', '2'],
		[' ', ' ', ' ', ' ', ' ', ' '],
		[' ', '4', ' ', ' ', '2', ' ']
	]).



%--------------------------------------------------------%
%-------Functions to display a board of any size---------%
%--------------------------------------------------------%

displayBoard([]).
displayBoard([Head | Tail]):- 
	length(Head, Size),
	displayBorder(Size),
	displayRows([Head | Tail]).

displayBorder(0):- write('-'), nl.
displayBorder(Number):-
	write('----'),
	NextNumber is Number - 1,
	displayBorder(NextNumber).

displayRows([]).
displayRows([Head | Tail]):-
	displayRow(Head),
	length(Head, Size),
	displayBorder(Size),
	displayRows(Tail).

displayRow([]):- 
	write('|'), nl.
displayRow([Head | Tail]):-
	write('| '), write(Head), write(' '), displayRow(Tail).


%--------------------------------------------------------%
%------Functions to generate a board of any size---------%
%--------------------------------------------------------%

generateBoard(0, _, []).
generateBoard(RowsLeft, RowSize, ResultBoard):-
	length(TempRow, RowSize),
	generateRow(TempRow, ResultRow),
	NextRowsLeft is RowsLeft - 1,
	generateBoard(NextRowsLeft, RowSize, R),
	append([ResultRow], R, ResultBoard).


generateRow([], []).
generateRow([Head | Tail], [' ' | RTail]):- generateRow(Tail, RTail).