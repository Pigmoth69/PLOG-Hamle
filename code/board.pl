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



%--------------------------------------------------------%
%-----------Functions to deploy the black pieces---------%
%--------------------------------------------------------%

deployBlacks(Board, _, 0, Board).
deployBlacks(Board, Size, NumberBlacks, BoardWithBlacks):-
	Max is Size + 1,
	random(1, Max, Row), random(1, Max, Col),
	NextRow is Row + 1,
	NextCol is Col + 1,
	PrevRow is Row - 1, 
	PrevCol is Col - 1,
	checkIfPositionIsFree(Row, Col, Max, Board),
	checkIfPositionIsFree(NextRow, Col, Max, Board),
	checkIfPositionIsFree(Row, NextCol, Max, Board),
	checkIfPositionIsFree(PrevRow, Col, Max, Board),
	checkIfPositionIsFree(Row, PrevCol, Max, Board),
	setPosition(Row, Col, 1, Board, ResultBoard),
	NewRowNumberBlacks is NumberBlacks - 1,
	deployBlacks(ResultBoard, Size, NewRowNumberBlacks, BoardWithBlacks).
deployBlacks(Board, Size, NumberBlacks, BoardWithBlacks):-
	deployBlacks(Board, Size, NumberBlacks, BoardWithBlacks).



%--------------------------------------------------------%
%-----------Functions to checks if free cell-------------%
%--------------------------------------------------------%

checkIfPositionIsFree(Row, Col, Size, _):-
	Col = Size; Col = 0; Row = Size; Row = 0.
checkIfPositionIsFree(Row, Col, Size, Board):-
	nth1(Row, Board, R),
	nth1(Col, R, 0).

%--------------------------------------------------------%
%-----------Functions to move black pieces---------------%
%--------------------------------------------------------%


moveBlacks(Row, _, _, Board, Size, Board):-
	Row > Size.
moveBlacks(Row, Col, BoardWithBlacks, Board, Size, ResultBoard):-
	Col > Size, 
	NextRow is Row + 1,
	print('extra col'), nl,
	moveBlacks(NextRow, 1, BoardWithBlacks, Board, Size, ResultBoard).
moveBlacks(Row, Col, BoardWithBlacks, Board, Size, ResultBoard):-
	print(Row), write(' '), print(Col), nl,
	print('blacks'), nl,
	nth1(Row, BoardWithBlacks, R),
	nth1(Col, R, 1),
	generateMovement(Row, Col, Board, Size, NewBoard),
	NextCol is Col + 1,
	moveBlacks(Row, NextCol, BoardWithBlacks, NewBoard, Size, ResultBoard).
moveBlacks(Row, Col, BoardWithBlacks, Board, Size, ResultBoard):-
	NextCol is Col + 1,
	moveBlacks(Row, NextCol, BoardWithBlacks, Board, Size, ResultBoard).

generateMovement(Row, Col, Board, Size, NewBoard):-
	print('generating'), nl,
	random(1, 5, Orientation),
	move(Orientation, Row, Col, Board, Size, NewBoard).

%mover para a direita
move(1, Row, Col, Board, Size, NewBoard):-
	print('right'), nl,
	Col < Size,
	Max is Size - Col + 1,
	random(1, Max, Delta),
	NewCol is Col + Delta,
	checkIfPositionIsFree(Row, NewCol, Size, Board),
	setPosition(Row, NewCol, Delta, Board, NewBoard).
%mover para a esquerda
move(2, Row, Col, Board, Size, NewBoard):-
	print('left'), nl,
	Col > 1,
	Max is Col,
	random(1, Max, Delta),
	NewCol is Col - Delta,
	checkIfPositionIsFree(Row, NewCol, Size, Board),
	setPosition(Row, NewCol, Delta, Board, NewBoard).
%mover para cima
move(3, Row, Col, Board, Size, NewBoard):-
	print('up'), nl,
	Row > 1,
	Max is Row,
	random(1, Max, Delta),
	NewRow is Row - Delta,
	checkIfPositionIsFree(NewRow, Col, Size, Board),
	setPosition(NewRow, Col, Delta, Board, NewBoard).
%mover para baixo
move(4, Row, Col, Board, Size, NewBoard):-
	print('down'), nl,
	Row < Size,
	Max is Size - Row + 1,
	random(1, Max, Delta),
	NewRow is Row + Delta,
	checkIfPositionIsFree(NewRow, Col, Size, Board),
	setPosition(NewRow, Col, Delta, Board, NewBoard).

move(_,Row, Col, Board, Size, NewBoard):-
	print('fail'), nl,
	generateMovement(Row, Col, Board, Size, NewBoard).


















flattenBoard([], List, List).
flattenBoard([Head | Tail], List, BoardList):-
	append(List, Head, Result),
	flattenBoard(Tail, Result, BoardList).