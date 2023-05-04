:- module(proylcc, 
	[  
		join/4
	]).


/**
 * join(+Grid, +NumOfColumns, +Path, -RGrids) 
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 */ 

/*
join(Grid, _NumOfColumns, _Path, RGrids):-
	Grid = [N | Ns],	% La implementación actual es simplemente a modo de muestra, y no tiene sentido, debe reepmplazarla
	N2 is N * 2,		% por una implementación válida.
	RGrids = [[0 | Ns], [N2 | Ns]].
*/

/*
Grilla = [B1, B2, B3, ..., Bn]
Path = [[X1, Y1], [X2, Y2], [X3, Y3], ... [Xn, Yn]]
Bloque N = Grilla[Path[N][0]*NumOfColumns + Path[N][1]]
aux = 0
Por cada Bloque Bi en Path:
	Buscarlo en la grilla,
	aux += valor del bloque,
	Reemplazar el bloque por 0,
	si es el último del camino
		reemplazarlo por la potencia de 2 más próxima a aux.
		buscarZeros().
*/

/*
join(Grid, NumOfColumns, Path, RGrids):-
	Path = [[X1|Y1]|[[X2|Y2]|_]],
	Index is X1*NumOfColumns+Y1,
	Index2 is X2*NumOfColumns+Y2,
	removeByIndex(Grid, Index, Grid1),
	removeByIndex(Grid1, Index2, Grid2),
	RGrids = [Grid2, Grid2].
*/

join(Grid, NumOfColumns, Path, RGrids):-
	removePath(Grid, NumOfColumns, Path, RGrids, 0).

setByIndex([_|Xs], 0, Num, [Num|Xs]).
setByIndex([X|Xs], Index, Num, [X|Res]):-
	Indexaux is Index-1,
	setByIndex(Xs, Indexaux, Num, Res).

searchByIndex([X|_], 0, X).
searchByIndex([_|Xs], Index, Res):-
	Indexaux is Index-1,
	searchByIndex(Xs, Indexaux, Res).

/*
removePath(Grid, NumOfColumns, [], [Grid, Xs], _):-
	bajar(Grid, NumOfColumns, 0, Xs).
removePath(Grid, NumOfColumns, Path, RGrids, Sum):-
	Path = [[X|Y] | Xs],                                     %
	isEmpty(Xs),                                             %		Si no hay más camino
	Index is X*NumOfColumns+Y,                               %
	searchByIndex(Grid, Index, Aux),                         %
	Sumaux is Sum+Aux,                                       %
	nextPower(Sumaux, 2, Pot),								 %
	setByIndex(Grid, Index, Pot, Gridaux),                   %		Creo el último bloque,
	removePath(Gridaux, NumOfColumns, Xs, RGrids, Pot);      %		Sino...
	Path = [[X|Y] | Xs],                                     %
	Index is X*NumOfColumns+Y,                               %
	searchByIndex(Grid, Index, Aux),                         %
	Sumaux is Sum+Aux,                                       %
	setByIndex(Grid, Index, 0, Gridaux),                     %		Lo pongo en 0
	removePath(Gridaux, NumOfColumns, Xs, RGrids, Sumaux).   %

isEmpty([]).

isGreater(X, Y):-
	X>Y.

nextPower(Num, Index, Res):-
	pow(2, Index, Aux),
	isGreater(Num, Aux),
	Indexaux is Index+1,
	nextPower(Num, Indexaux, Res);
	pow(2, Index, Aux),
	Res is Aux.

pow(X, Y, Z):-
	Z is X**Y.

bajar(Grid, _, 40, Res):- 								% Si llego al último índice, termino.
	Res = Grid.

bajar(Grid, NumOfColumns, Index, RGrids):-
	nth0(Index, Grid, 0),								% Si encuentro un 0
	bajarCol(Grid, NumOfColumns, Index, Res),			% Bajo toda la columna un bloque para cubrir el 0
	Indexaux is Index+1,								% Actualizo el índice
	bajar(Res, NumOfColumns, Indexaux, RGrids).			% Paso al siguiente índice

bajar(Grid, NumOfColumns, Index, RGrids):-
	Indexaux is Index+1,								% Si no encuentro un 0
	bajar(Grid, NumOfColumns, Indexaux, RGrids).		% Paso al siguiente índice

bajarCol(Grid, NumOfColumns, Index, Res):-
	Index<NumOfColumns,									% Si estoy en la primer fila, ya terminé de bajar y 
	random(2, 2048, Aux),								% sí o sí tengo que insertar un nuevo bloque
	nextPower(Aux, 1, Num),								% Busco la próxima potencia de Aux
	setByIndex(Grid, Index, Num, Res).					% Creo el nuevo bloque.

bajarCol(Grid, NumOfColumns, Index, Res):-				% Si no estoy en la primera fila
	Indexaux is Index-NumOfColumns,						% Tomo el índice del bloque en la fila superior
	searchByIndex(Grid, Indexaux, Aux),					% Tomo su valor
	setByIndex(Grid, Index, Aux, GridN),				% Lo seteo en lugar del 0
	bajarCol(GridN, NumOfColumns, Indexaux, Res).		% Paso al bloque de arriba para bajar el resto
*/

removePath(Grid, NumOfColumns, [], [Grid, Xs, Ys], _):-
	bajar(Grid, NumOfColumns, 0, Xs),
	insertarNuevos(Xs, 0, Ys).
removePath(Grid, NumOfColumns, Path, RGrids, Sum):-
	Path = [[X|Y] | Xs],                                     %
	isEmpty(Xs),                                             %		Si no hay más camino
	Index is X*NumOfColumns+Y,                               %
	searchByIndex(Grid, Index, Aux),                         %
	Sumaux is Sum+Aux,                                       %
	nextPower(Sumaux, 2, Pot),								 %
	setByIndex(Grid, Index, Pot, Gridaux),                   %		Creo el último bloque,
	removePath(Gridaux, NumOfColumns, Xs, RGrids, Pot);      %		Sino...
	Path = [[X|Y] | Xs],                                     %
	Index is X*NumOfColumns+Y,                               %
	searchByIndex(Grid, Index, Aux),                         %
	Sumaux is Sum+Aux,                                       %
	setByIndex(Grid, Index, 0, Gridaux),                     %		Lo pongo en 0
	removePath(Gridaux, NumOfColumns, Xs, RGrids, Sumaux).   %

isEmpty([]).

nextPower(Num, Index, Res):-
	pow(2, Index, Aux),
	isGreater(Num, Aux),
	Indexaux is Index+1,
	nextPower(Num, Indexaux, Res);
	pow(2, Index, Aux),
	Res is Aux.

isGreater(X, Y):-
	X>Y.

pow(X, Y, Z):-
	Z is X**Y.

bajar(Grid, _, 40, Res):- 								% Si llego al último índice, termino.
	Res = Grid.

bajar(Grid, NumOfColumns, Index, RGrids):-
	nth0(Index, Grid, 0),								% Si encuentro un 0
	bajarCol(Grid, NumOfColumns, Index, Res),			% Bajo toda la columna un bloque para cubrir el 0
	Indexaux is Index+1,								% Actualizo el índice
	bajar(Res, NumOfColumns, Indexaux, RGrids).			% Paso al siguiente índice

bajar(Grid, NumOfColumns, Index, RGrids):-
	Indexaux is Index+1,								% Si no encuentro un 0
	bajar(Grid, NumOfColumns, Indexaux, RGrids).		% Paso al siguiente índice

bajarCol(Grid, NumOfColumns, Index, Res):-
	Index<NumOfColumns,									% Si estoy en la primer fila, ya terminé de bajar y 
	setByIndex(Grid, Index, 0, Res).					% Creo el nuevo bloque.

bajarCol(Grid, NumOfColumns, Index, Res):-				% Si no estoy en la primera fila
	Indexaux is Index-NumOfColumns,						% Tomo el índice del bloque en la fila superior
	searchByIndex(Grid, Indexaux, Aux),					% Tomo su valor
	setByIndex(Grid, Index, Aux, GridN),				% Lo seteo en lugar del 0
	bajarCol(GridN, NumOfColumns, Indexaux, Res).		% Paso al bloque de arriba para bajar el resto

insertarNuevos(Grid, 40, Res):-
	Res = Grid.

insertarNuevos(Grid, Index, Res):-
	nth0(Index, Grid, 0),
	random(1, 10, Aux),								% sí o sí tengo que insertar un nuevo bloque
	pow(2, Aux, Num),
	setByIndex(Grid, Index, Num, GridN),				% Creo el nuevo bloque.
	Indexaux is Index+1,
	insertarNuevos(GridN, Indexaux, Res).

insertarNuevos(Grid, Index, Res):-
	Indexaux is Index+1,
	insertarNuevos(Grid, Indexaux, Res).