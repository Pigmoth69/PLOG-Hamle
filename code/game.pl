defaultGame:- 
	defaultBoard(Board),
	displayBoard(Board).

newGame:- 
	getBoardSize(Size),
	generateBoard(Size,Size,Board),
	displayBoard(Board).