:- module(proylcc, 
	[  
		join/4
	]).

/*
	join(Grid, NumOfColumns, Path, RGrids) 
	RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
	en la grilla Grid, con número de columnas NumOfColumns.
	join llama a removePath() que se encarga de vaciar el camino, hacer el efecto de gravedad y añadir los
	nuevos bloques.
	El número 0 representa que la celda está vacía.
*/ 

join(Grid, NumOfColumns, Path, RGrids):-
	removePath(Grid, NumOfColumns, Path, RGrids, 0).

/*
	setByIndex(Lista, Index, Num, LRes):
	Busca el índice "Index" en la lista y reemplaza su valor por el de "Num", luego retorna la nueva lista
	mediante LRes.
*/
setByIndex([_|Xs], 0, Num, [Num|Xs]).
setByIndex([X|Xs], Index, Num, [X|Res]):-
	Indexaux is Index-1,
	setByIndex(Xs, Indexaux, Num, Res).

/*
	searchByIndex(Lista, Index, Res):
	Busca el índice "Index" en la lista y lo devuelve como Res.
*/
searchByIndex([X|_], 0, X).
searchByIndex([_|Xs], Index, Res):-
	Indexaux is Index-1,
	searchByIndex(Xs, Indexaux, Res).

/*
	removePath(Grid, NumOfColumns, Path, RGrids, Sum):
	Caso base: cuando el camino está vacío porque significa que ya se eliminaron todos los bloques del mismo.
	Llama a bajar() que se encarga de hacer el efecto de gravedad y a  insertarNuevos() que genera los nuevos
	bloques en los espacios vacíos. Luego, tanto la grilla con los bloques eliminados, la grilla con el efecto
	de gravedad y la grilla final con los nuevos bloques se añaden en RGrids.
*/
removePath(Grid, NumOfColumns, [], [Grid, Xs, Ys], _):-
	bajar(Grid, NumOfColumns, 0, Xs),
	insertarNuevos(Xs, 0, Ys).

/*
	removePath(Grid, NumOfColumns, Path, RGrids, Sum):
	Segundo caso recursivo: si estoy en el último bloque del camino.
	Algoritmo:
	- Tomar el bloque, verificar que sea el último del camino y calcular su índice en la grilla.
	- Buscar su valor y actualizar la suma.
	- Buscar la menor potencia de 2 mayor o igual a la suma y crear el nuevo bloque con ese valor.
	- Llamar al caso final con la lista vacía.
*/
removePath(Grid, NumOfColumns, Path, RGrids, Sum):-
	Path = [[X|Y] | Xs],
	isEmpty(Xs),
	Index is X*NumOfColumns+Y,
	searchByIndex(Grid, Index, Aux),
	Sumaux is Sum+Aux,
	nextPower(Sumaux, 2, Pot),
	setByIndex(Grid, Index, Pot, Gridaux),
	removePath(Gridaux, NumOfColumns, Xs, RGrids, Pot).

/*
	removePath(Grid, NumOfColumns, Path, RGrids, Sum):
	"Grid" es la grilla original, "NumOfColumns" el número de columnas de la grilla, "Path" el camino a eliminar,
	"RGrids" la lista de grillas resultantes y "Sum" la suma de los valores de los bloques, es decir, los puntos.
	Primer caso recursivo: si al camino le quedan al menos dos bloques.
	Algoritmo:
	- Agarrar el primer bloque del camino y calcular su índice en la grilla.
	- Buscar su valor y actualizar la suma.
	- Eliminar el bloque.
	- Llamar recursivamente con el resto del camino.
*/
removePath(Grid, NumOfColumns, Path, RGrids, Sum):-
	Path = [[X|Y] | Xs],
	Index is X*NumOfColumns+Y,
	searchByIndex(Grid, Index, Aux),
	Sumaux is Sum+Aux,
	setByIndex(Grid, Index, 0, Gridaux),
	removePath(Gridaux, NumOfColumns, Xs, RGrids, Sumaux).

/*
	isEmpty(Lista):
	Devuelve true si "Lista" está vacía.
*/
isEmpty([]).

/*
	nextPower(Num, Index, Res):
	Dado un número "Num" y un índice "Index", calcula la menor potencia de 2 mayor o igual a Num
	y la devuelve a través de "Res".
	Caso recursivo: si Num es mayor a 2^Index.
		Busca la próxima potencia de 2 aumentando Index en 1.
	Caso base: si Num es menor o igual a 2^Index.
		Entonces, ya encontró el resultado.
*/
nextPower(Num, Index, Res):-
	pow(2, Index, Aux),
	isGreater(Num, Aux),
	Indexaux is Index+1,
	nextPower(Num, Indexaux, Res);
	pow(2, Index, Aux),
	Res is Aux.

/*
	isGreater(X, Y):
	Devuelve true si X > Y.
*/
isGreater(X, Y):-
	X>Y.

/*
	pow(X, Y, Res):
	Calcula X^Y y lo devuelve mediante "Res".
*/
pow(X, Y, Res):-
	Res is X**Y.

/*
	bajar(Grid, NumOfColumns, Index, Res):
	Caso final: cuando el índice llega a 40, ya que es el primero que está fuera de la grilla.
		Devuelve como "Res" la grilla actual.
*/
bajar(Grid, _, 40, Res):-
	Res = Grid.

/*
	bajar(Grid, NumOfColumns, Index, RGrids):
	Caso recursivo 2: el bloque perteneciente al índice actual es 0.
		Llamar a bajarCol que se encarga de bajar todo lo que hay encima del 0 un bloque hacia abajo,
		cubriéndolo y dejando el espacio vacío arriba para el nuevo bloque.
*/
bajar(Grid, NumOfColumns, Index, RGrids):-
	nth0(Index, Grid, 0),
	bajarCol(Grid, NumOfColumns, Index, Res),
	Indexaux is Index+1,
	bajar(Res, NumOfColumns, Indexaux, RGrids).

/*
	bajar(Grid, NumOfColumns, Index, RGrids):
	Caso recursivo 1: el bloque perteneciente al índice actual no es 0.
		Entonces, paso al siguiente índice.
*/
bajar(Grid, NumOfColumns, Index, RGrids):-
	Indexaux is Index+1,
	bajar(Grid, NumOfColumns, Indexaux, RGrids).

/*
	bajarCol(Grid, NumOfColumns, Index, Res)
	Caso base: estoy en la primer fila, significa que ya baje todos los bloques.
		Dejarlo como vacío para poder añadir el bloque nuevo.
*/
bajarCol(Grid, NumOfColumns, Index, Res):-
	Index<NumOfColumns,
	setByIndex(Grid, Index, 0, Res).

/*
	bajarCol(Grid, NumOfColumns, Index, Res)
	Caso recursivo: aún no llego a la primer fila.
		Tomar el valor del bloque encima del actual y ponerlo como valor actual.
		Llamar recursivamente con el índice perteneciente al bloque superior al actual.
*/
bajarCol(Grid, NumOfColumns, Index, Res):-
	Indexaux is Index-NumOfColumns,
	searchByIndex(Grid, Indexaux, Aux),
	setByIndex(Grid, Index, Aux, GridN),
	bajarCol(GridN, NumOfColumns, Indexaux, Res).

insertarNuevos(Grid, 40, Res):-
	Res = Grid.

insertarNuevos(Grid, Index, Res):-
	nth0(Index, Grid, 0),
	random(1, 10, Aux),
	pow(2, Aux, Num),
	setByIndex(Grid, Index, Num, GridN),
	Indexaux is Index+1,
	insertarNuevos(GridN, Indexaux, Res).

insertarNuevos(Grid, Index, Res):-
	Indexaux is Index+1,
	insertarNuevos(Grid, Indexaux, Res).

boost(Grid, NumOfColumns, _Sum, _RGrids):-
	split_every(NumOfColumns, Grid, _Filas).
	%boostAux(Grid, NumOfColumns, Sum, 0, RGrids).

split_every(_, [], []).
split_every(N, L, [Fila|Filas]) :-
    length(Fila, N),
    append(Fila, Resto, L),
    split_every(N, Resto, Filas).

/*
boostAux(Grid, NumOfColumns, Sum, Index, RGrids, [[X1|Xs]|Ys]):-
	searchByIndex(Grid, Index, X),
	Indexaux = Index+1,
	(Indexaux mod NumOfColumns)>(Index mod NumOfColumns),
	searchByIndex(Grid, Indexaux, Y),
	X =:= Y,
	boostAux(Grid, NumOfColumns, Sum, Indexaux, RGrids).

boostAux(Grid, NumOfColumns, Sum, Index, RGrids):-
	searchByIndex(Grid, Index, X),
	Indexaux = Index+1.

tieneAdy(Grid, NumOfColumns, 0):-
	searchByIndex(Grid, 0, X),
	right(Grid, NumOfColumns, 0, X);
	searchByIndex(Grid, 0, X),
	bottom(Grid, NumOfColumns, 0, X);
	searchByIndex(Grid, 0, X),
	bottomRight(Grid, NumOfColumns, 0, X).

tieneAdy(Grid, NumOfColumns, 5):-
	searchByIndex(Grid, 5, X),
	left(Grid, NumOfColumns, 5, X);
	searchByIndex(Grid, 5, X),
	bottomLeft(Grid, NumOfColumns, 5, X);
	searchByIndex(Grid, 5, X),
	bottom(Grid, NumOfColumns, 5, X).

tieneAdy(Grid, NumOfColumns, Index):-
	Index<5,
	Index>0,
	searchByIndex(Grid, Index, X),
	adyPrimeraFila(Grid, NumOfColumns, Index, X).

adyEsquina1(Grid, NumOfColumns, Index, X):-
	right(Grid, NumOfColumns, Index, X);
	bottom(Grid, NumOfColumns, Index, X);
	bottomRight(Grid, NumOfColumns, Index, X).

adyEsquina2(Grid, NumOfColumns, Index, X):-
	left(Grid, NumOfColumns, Index, X);
	bottom(Grid, NumOfColumns, Index, X);
	bottomLeft(Grid, NumOfColumns, Index, X).

adyEsquina3(Grid, NumOfColumns, Index, X):-
	top(Grid, NumOfColumns, Index, X);
	topRight(Grid, NumOfColumns, Index, X);
	right(Grid, NumOfColumns, Index, X).

adyEsquina4(Grid, NumOfColumns, Index, X):-
	top(Grid, NumOfColumns, Index, X);
	topLeft(Grid, NumOfColumns, Index, X);
	left(Grid, NumOfColumns, Index, X).

adyPrimeraColumna(Grid, NumOfColumns, Index, X):-
	top(Grid, NumOfColumns, Index, X);
	topRight(Grid, NumOfColumns, Index, X);
	right(Grid, NumOfColumns, Index, X);
	bottom(Grid, NumOfColumns, Index, X);
	bottomRight(Grid, NumOfColumns, Index, X).

adyUltimaColumna(Grid, NumOfColumns, Index, X):-
	top(Grid, NumOfColumns, Index, X);
	topLeft(Grid, NumOfColumns, Index, X);
	left(Grid, NumOfColumns, Index, X);
	bottom(Grid, NumOfColumns, Index, X);
	bottomLeft(Grid, NumOfColumns, Index, X).

adyUltimaFila(Grid, NumOfColumns, Index, X):-
	topLeft(Grid, NumOfColumns, Index, X);
	top(Grid, NumOfColumns, Index, X);
	topRight(Grid, NumOfColumns, Index, X);
	left(Grid, NumOfColumns, Index, X);
	right(Grid, NumOfColumns, Index, X).

adyPrimeraFila(Grid, NumOfColumns, Index, X):-
	left(Grid, NumOfColumns, Index, X);
	right(Grid, NumOfColumns, Index, X);
	bottomLeft(Grid, NumOfColumns, Index, X);
	bottom(Grid, NumOfColumns, Index, X);
	bottomRight(Grid, NumOfColumns, Index, X).

adyNormal(Grid, NumOfColumns, Index, X):-
	topLeft(Grid, NumOfColumns, Index, X);
	top(Grid, NumOfColumns, Index, X);
	topRight(Grid, NumOfColumns, Index, X);
	left(Grid, NumOfColumns, Index, X);
	right(Grid, NumOfColumns, Index, X);
	bottomLeft(Grid, NumOfColumns, Index, X);
	bottom(Grid, NumOfColumns, Index, X);
	bottomRight(Grid, NumOfColumns, Index, X).

topLeft(Grid, NumOfColumns, Index, Elem):-
	IndexTL is Index-NumOfColumns-1,
	searchByIndex(Grid, IndexTL, TL),
	Elem =:= TL.

top(Grid, NumOfColumns, Index, Elem):-
	IndexT is Index-NumOfColumns,
	searchByIndex(Grid, IndexT, Top),
	Elem =:= Top.

topRight(Grid, NumOfColumns, Index, Elem):-
	IndexTR is Index-NumOfColumns+1,
	searchByIndex(Grid, IndexTR, TR),
	Elem =:= TR.

left(Grid, NumOfColumns, Index, Elem):-
	IndexL is Index-1,
	searchByIndex(Grid, IndexL, Left),
	Elem =:= Left.

right(Grid, NumOfColumns, Index, Elem):-
	IndexR is Index+1,
	searchByIndex(Grid, IndexR, Right),
	Elem =:= Right.

bottomLeft(Grid, NumOfColumns, Index, Elem):-
	IndexBL is Index+NumOfColumns-1,
	searchByIndex(Grid, IndexBL, BL),
	Elem =:= BL.

bottom(Grid, NumOfColumns, Index, Elem):-
	IndexB is Index+NumOfColumns,
	searchByIndex(Grid, IndexB, Bottom),
	Elem =:= Bottom.

bottomRight(Grid, NumOfColumns, Index, Elem):-
	IndexBR is Index+NumOfColumns+1,
	searchByIndex(Grid, IndexBR, BR),
	Elem =:= BR.
*/