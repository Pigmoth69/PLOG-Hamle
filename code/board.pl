defaultInfoList(
	[
		0, 3, 0, 0, 0, 2,
		0, 0, 3, 0, 4, 0,
		0, 1, 0, 0, 0, 0,
		5, 0, 0, 2, 0, 2,
		0, 0, 0, 0, 0, 0,
		0, 4, 0, 2, 0, 0
	]).

defaultIndexList(
	[
		0, 0, 0, 1, 0, 0,
		1, 0, 0, 0, 0, 1,
		0, 0, 1, 0, 0, 0,
		0, 1, 0, 1, 0, 1,
		0, 0, 0, 0, 0, 0,
		0, 1, 0, 1, 0, 1
	]).


setUpBoard([], _, []).
setUpBoard(List, N, [Row | TBoard]):-
	length(List, Length),
	NewLength is Length - N, 
	sublist(List, Row, 0, N, NewLength),
	sublist(List, NewList, N, NewLength, 0),
	setUpBoard(NewList, N, TBoard).

setFinalBoard(ClearedBoard, _, _, _, [], _, ClearedBoard).
setFinalBoard(ClearedBoard, Info, Index, [Head | Tail], [BHead | BTail], N, FinalBoard):-
	setFinalBoard(ClearedBoard, Info, Index, Tail, BTail, N, F),
	TempRow is Head//N, Row is TempRow+1, TempCol is Head mod N, Col is TempCol + 1,
	nth1(Pos, Index, BHead),
	nth1(Pos, Info, Value),
	write(Head), write(' '), write(Row), write(' '),  write(Col), nl,
	setPosition(Row, Col, Value, F, FinalBoard).




%--------------------------------------------------------%
%-------Functions to display a board of any size---------%
%--------------------------------------------------------%

displayBoard([]).
displayBoard([Head | Tail]):- 
	length(Head, Size),
	write('   '), displayColIndexes(Size, 0), nl,
	write('   '), displayBorder(Size),
	displayRows([Head | Tail], 97).

displayBorder(0):- write('-'), nl.
displayBorder(Number):-
	write('----'),
	NextNumber is Number - 1,
	displayBorder(NextNumber).

displayRows([], _).
displayRows([Head | Tail], RowNumber):-
	displayRowIndex(RowNumber),
	length(Head, Size),
	displayRow(Head),
	write('   '), displayBorder(Size),
	NewRowNumber is RowNumber + 1,
	displayRows(Tail, NewRowNumber).

displayRow([]):- 
	write('|'), nl.
displayRow([0 | Tail]):-
	write('| '), write(' '), write(' '), displayRow(Tail).
displayRow([Head | Tail]):-
	write('| '), write(Head), write(' '), displayRow(Tail).

displayColIndexes(Size, Size).
displayColIndexes(Size, Index):-
	NewIndex is Index + 1,
	NewIndex < 10,
	write('  '), write(NewIndex),  write(' '),
	displayColIndexes(Size, NewIndex).
displayColIndexes(Size, Index):-
	NewIndex is Index + 1,
	write('  '), write(NewIndex),
	displayColIndexes(Size, NewIndex).

displayRowIndex(CharCode):- format(' ~c ', [CharCode]).

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
generateRow([_ | Tail], [0 | RTail]):- generateRow(Tail, RTail).



%--------------------------------------------------------%
%-----------Functions to edit the board info-------------%
%--------------------------------------------------------%

	%CLEAR %POSITION
clearPosition(_, _, [], _):- nl, write('clearPosition out of range.'), abort.

clearPosition(0, 1, [_ | Tail], [' ' | Tail]).

clearPosition(0, Col, [Head | Tail], [Head | RTail]):-
	NewCol is Col - 1,
	clearPosition(0, NewCol, Tail, RTail).

clearPosition(1, Col, [Head | Tail], [RHead | Tail]):-
	clearPosition(0, Col, Head, RHead).

clearPosition(Row, Col, [Head | Tail], [Head | RTail]):-
	NewRow is Row - 1,
	clearPosition(NewRow, Col, Tail, RTail).

	%SET %POSITION
%setPosition(_, _, _, [], _):- nl, write('setPosition out of range.'), abort.

setPosition(0, 1, Info, [_ | Tail], [Info | Tail]).

setPosition(0, Col, Info, [Head | Tail], [Head | RTail]):-
	NewCol is Col - 1,
	setPosition(0, NewCol, Info, Tail, RTail).

setPosition(1, Col, Info, [Head | Tail], [RHead | Tail]):-
	setPosition(0, Col, Info, Head, RHead).

setPosition(Row, Col, Info, [Head | Tail], [Head | RTail]):-
	NewRow is Row - 1,
	setPosition(NewRow, Col, Info, Tail, RTail).

	%CLEAR %BOARD 
clearBoard([], []).
clearBoard([Head | Tail], [RHead | RTail]):-
	clearRow(Head, RHead),
	clearBoard(Tail, RTail).

	%CLEAR %ROW 
clearRow([], []).
clearRow([_ | Tail], [' ' | RTail]):-
	clearRow(Tail, RTail).


	%GET %POSITION
getPosition(Row, Col, Info, Board):-
	nth1(Row, Board, List),
	element(Col, List, Info).