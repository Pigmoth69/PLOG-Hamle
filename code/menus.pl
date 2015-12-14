mainMenu:- 

	printMainMenu,
	getChar(Input),
	(
		Input = '1' -> defaultGame, getEnter, mainMenu;
		Input = '2' -> newGame, getEnter, mainMenu;
		Input = '3' -> howTo, mainMenu;
		Input = '4' -> abort;
		abort
	).

printMainMenu:-
	clearScreen,
	write('*********************************'), nl,
	write('||             HAMLE           ||'), nl,
	write('||                             ||'), nl,
	write('||  1 - Resolve Default Board  ||'), nl,
	write('||  2 - Create New Game        ||'), nl,
	write('||  3 - How to                 ||'), nl,
	write('||  4 - Exit Game              ||'), nl,
	write('||                             ||'), nl,
	write('*********************************'), nl,
	write('Choose an option:'), nl.



howTo:-
	clearScreen,
	write('**********************************************************************'), nl,
	write('||                        How to play HAMLE                         ||'), nl,
	write('**********************************************************************'), nl,
	write('||                                                                  ||'), nl,
	write('||      Move every black square in one of the four directions, so   ||'), nl,
	write('||  that the number in the black cells indicates the length of      ||'), nl,
	write('||  their moves.                                                    ||'), nl,
	write('||                                                                  ||'), nl,
	write('||      When all moves are done, all white cells should be inter-   ||'), nl,
	write('||  connected and black cells should not touch each other from the  ||'), nl,
	write('||  sides.                                                          ||'), nl,
	write('||                                                                  ||'), nl,
	write('**********************************************************************'), nl,
	getEnter.



getBoardSize(Size):- 
	printGetBoardSize,
	getDoubleDigitInteger(Size).

printGetBoardSize:-
	clearScreen,
	write('*********************************'), nl,
	write('||          New Game           ||'), nl,
	write('*********************************'), nl,
	write('Insert the board dimension(XX): ').

getLabelingType(Type):- 
	printGetLabelingType,
	getInteger(Type), 
	Type < 3.

printGetLabelingType:-
	clearScreen,
	write('**************************************'), nl,
	write('||          Labeling type           ||'), nl,
	write('||   1 - Default                    ||'), nl,
	write('||   2 - FFC                        ||'), nl,
	write('||                                  ||'), nl,
	write('**************************************'), nl,
	write('Insert labeling type: ').