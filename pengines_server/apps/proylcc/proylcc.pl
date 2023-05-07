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
	setByIndex(Grid, Index, Num, Res):
	Busca el índice "Index" en la grilla y reemplaza su valor por el de "Num", luego retorna la nueva lista
	mediante Res.
*/

setByIndex(Grid, Index, Num, Res):-
	nth0(Index, Grid, _, Tail),
    nth0(Index, Res, Num, Tail).

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
	nth0(Index, Grid, Aux),
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
	nth0(Index, Grid, Aux),
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
	nth0(Indexaux, Grid, Aux),
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

boost(Grid, NumOfColumns, RGrids):-
	booster(Grid, NumOfColumns, 0, [], RGridsAux, []), 		% Llamo a booster
	RGrids = RGridsAux.

borrar(Grid, [], _, Grid, List, List).

borrar(Grid, [X|Ys], Tam, Res, List, ListR):-
	isEmpty(Ys),
	nth0(X, Grid, Aux),							% Tomo su valor
	Sumaux is Aux*Tam,
	nextPower(Sumaux, 2, Pot),
	concatenar_listas([X, Pot], List, ListR),
	setByIndex(Grid, X, 0, Res).				% Añado el bloque final

borrar(Grid, [X|Ys], Tam, Res, List, ListR):-				% Si hay más adyacentes
	setByIndex(Grid, X, 0, GridAux),			% Setteo en 0 y paso al siguiente.
	borrar(GridAux, Ys, Tam, Res, List, ListR).

concatenar_listas([X,Y], [], [[X,Y]]).
concatenar_listas([X,Y], L, [[X,Y]|L]):- 
	\+ isEmpty(L).
concatenar_listas([X1,Y1|T1], [H2|T2], [[X1,Y1]|Res]):-
	concatenar_listas(T1, [H2|T2], Res).

booster(Grid, NumOfColumns, 40, _, [Res, Xs, Ys], List):-
	insertList(Grid, List, Res),
	bajar(Res, NumOfColumns, 0, Xs),
	insertarNuevos(Xs, 0, Ys).

booster(Grid, NumOfColumns, Index, NoVisit, GridR, List):-
	getAdjacent(Grid, NumOfColumns, Index, NoVisitAux),			% Tomo los adyacentes de index
	length(NoVisitAux, L),										% Tomo la cantidad de adyacentes
	borrar(Grid, NoVisitAux, L, Grid1, List, ListR),							% Actualizo la grilla
	append(NoVisit, NoVisitAux, Res),							% Junto las listas de adyacentes
	sort(Res, Sorted),											% Organizo la lista de adyacentes
	Indexaux is Index+1,										% Paso al siguiente
	\+ member(Indexaux, Sorted),								% Si no está en la lista de adyacentes visitados
	booster(Grid1, NumOfColumns, Indexaux, Sorted, GridR, ListR);		% Llamada recursiva a booster
	getAdjacent(Grid, NumOfColumns, Index, NoVisitAux),			% Si está en la lista de adyacentes visitados
	length(NoVisitAux, L),										% Tomo la cantidad de adyacentes
	borrar(Grid, NoVisitAux, L, Grid1, List, ListR),							% Actualizo la grilla
	append(NoVisit, NoVisitAux, Res),							% Vuelvo a repetir todo
	sort(Res, Sorted),
	Indexaux is Index+1,
	booster2(Grid1, NumOfColumns, Indexaux, Sorted, GridR, ListR).		% Y llamo a booster2

booster2(Grid, NumOfColumns, 40, _, [Res, Xs, Ys], List):-
	insertList(Grid, List, Res),
	bajar(Res, NumOfColumns, 0, Xs),
	insertarNuevos(Xs, 0, Ys).

booster2(Grid, NumOfColumns, Index, Sorted, GridR, List):-
	Indexaux is Index+1,
	\+ member(Indexaux, Sorted),						% Si no se visitó, llamo a booster
	booster(Grid, NumOfColumns, Indexaux, Sorted, GridR, List);
	Indexaux is Index+1,
	booster2(Grid, NumOfColumns, Indexaux, Sorted, GridR, List).		% Si se visitó, llamada recursiva a booster 2.

insertList(Grid, [], Grid).

insertList(Grid, [[X,Y]|Xs], Res):-
	setByIndex(Grid, X, Y, Aux),
	insertList(Aux, Xs, Res).

getAdjacent(Grid, NumOfColumns, Index, AdjacentList):-
	tieneAdy(Grid, NumOfColumns, Index, AdjacentIndexes),
	getAdjacentList(Grid, NumOfColumns, AdjacentIndexes, [], AdjacentListUn),
	sort(AdjacentListUn, AdjacentList).

getAdjacentList(_, _, [], AdjacentList, AdjacentList).
getAdjacentList(Grid, NumOfColumns, [Index|Rest], Acc, AdjacentList):-
	member(Index, Acc),
	getAdjacentList(Grid, NumOfColumns, Rest, Acc, AdjacentList).

getAdjacentList(Grid, NumOfColumns, [Index|Rest], Acc, AdjacentList):-
	\+ member(Index, Acc),
	tieneAdy(Grid, NumOfColumns, Index, AdjacentIndexes),
	append(Rest, AdjacentIndexes, NewList),
	getAdjacentList(Grid, NumOfColumns, NewList, [Index|Acc], AdjacentList).
/*
obtenerAdyacentesDeAdy(Grid, NumOfColumns, [Index|Resto], Res):-
    adyacentesDePosiciones(Grid, NumOfColumns, [Index|Resto], AdyList),
    list_to_set(AdyList, UniqueAdyList),
    exclude(member(PosList), UniqueAdyList, Res),
	obtenerAdyacentesDeAdy(Grid, NumOfColumns, Resto, Res).
*/
tieneAdy(Grid, NumOfColumns, 0, Res):-
	adyEsquina1(Grid, NumOfColumns, 0, Res).

tieneAdy(Grid, NumOfColumns, 4, Res):-
	adyEsquina2(Grid, NumOfColumns, 4, Res).

tieneAdy(Grid, NumOfColumns, 35, Res):-
	adyEsquina3(Grid, NumOfColumns, 35, Res).

tieneAdy(Grid, NumOfColumns, 39, Res):-
	adyEsquina4(Grid, NumOfColumns, 39, Res).

tieneAdy(Grid, NumOfColumns, Index, Res):-
	Index<NumOfColumns,
	adyPrimeraFila(Grid, NumOfColumns, Index, Res).

tieneAdy(Grid, NumOfColumns, Index, Res):-
	Index>35,
	Index<39,
	adyUltimaFila(Grid, NumOfColumns, Index, Res).

tieneAdy(Grid, NumOfColumns, Index, Res):-
	Index mod NumOfColumns =:= 0,
	adyPrimeraColumna(Grid, NumOfColumns, Index, Res).

tieneAdy(Grid, NumOfColumns, Index, Res):-
	Index mod NumOfColumns =:= 4,
	adyUltimaColumna(Grid, NumOfColumns, Index, Res).

tieneAdy(Grid, NumOfColumns, Index, Res):-
	adyNormal(Grid, NumOfColumns, Index, Res).

adyEsquina1(Grid, NumOfColumns, Index, Res):-
	findall(Valor, (right(Grid, NumOfColumns, Index, Valor);
					bottom(Grid, NumOfColumns, Index, Valor);
					bottomRight(Grid, NumOfColumns, Index, Valor)), Valores),
	Res = Valores.

adyEsquina2(Grid, NumOfColumns, Index, Res):-
	findall(Valor, (left(Grid, NumOfColumns, Index, Valor);
					bottom(Grid, NumOfColumns, Index, Valor);
					bottomLeft(Grid, NumOfColumns, Index, Valor)), Valores),
	Res = Valores.

adyEsquina3(Grid, NumOfColumns, Index, Res):-
	findall(Valor, (top(Grid, NumOfColumns, Index, Valor);
					topRight(Grid, NumOfColumns, Index, Valor);
					right(Grid, NumOfColumns, Index, Valor)), Valores),
	Res = Valores.

adyEsquina4(Grid, NumOfColumns, Index, Res):-
	findall(Valor, (top(Grid, NumOfColumns, Index, Valor);
					topLeft(Grid, NumOfColumns, Index, Valor);
					left(Grid, NumOfColumns, Index, Valor)), Valores),
	Res = Valores.

adyPrimeraColumna(Grid, NumOfColumns, Index, Res):-
	findall(Valor, (top(Grid, NumOfColumns, Index, Valor);
					topRight(Grid, NumOfColumns, Index, Valor);
					right(Grid, NumOfColumns, Index, Valor);
					bottom(Grid, NumOfColumns, Index, Valor);
					bottomRight(Grid, NumOfColumns, Index, Valor)), Valores),
	Res = Valores.

adyUltimaColumna(Grid, NumOfColumns, Index, Res):-
	findall(Valor, (top(Grid, NumOfColumns, Index, Valor);
					topLeft(Grid, NumOfColumns, Index, Valor);
					left(Grid, NumOfColumns, Index, Valor);
					bottom(Grid, NumOfColumns, Index, Valor);
					bottomLeft(Grid, NumOfColumns, Index, Valor)), Valores),
	Res = Valores.

adyUltimaFila(Grid, NumOfColumns, Index, Res):-
	findall(Valor, (topLeft(Grid, NumOfColumns, Index, Valor);
					top(Grid, NumOfColumns, Index, Valor);
					topRight(Grid, NumOfColumns, Index, Valor);
					left(Grid, NumOfColumns, Index, Valor);
					right(Grid, NumOfColumns, Index, Valor)), Valores),
	Res = Valores.

adyPrimeraFila(Grid, NumOfColumns, Index, Res):-
	findall(Valor, (left(Grid, NumOfColumns, Index, Valor);
					right(Grid, NumOfColumns, Index, Valor);
					bottomLeft(Grid, NumOfColumns, Index, Valor);
					bottom(Grid, NumOfColumns, Index, Valor);
					bottomRight(Grid, NumOfColumns, Index, Valor)), Valores),
	Res = Valores.

adyNormal(Grid, NumOfColumns, Index, Res):-
	findall(Valor, (topLeft(Grid, NumOfColumns, Index, Valor);
                    top(Grid, NumOfColumns, Index, Valor);
                    topRight(Grid, NumOfColumns, Index, Valor);
                    left(Grid, NumOfColumns, Index, Valor);
                    right(Grid, NumOfColumns, Index, Valor);
                    bottomLeft(Grid, NumOfColumns, Index, Valor);
                    bottom(Grid, NumOfColumns, Index, Valor);
                    bottomRight(Grid, NumOfColumns, Index, Valor)), Valores),
    Res = Valores.

topLeft(Grid, NumOfColumns, Index, IndexRes):-
	IndexTL is Index-NumOfColumns-1,
	nth0(Index, Grid, Elem),
	nth0(IndexTL, Grid, TL),
	Elem =:= TL,
	IndexRes = IndexTL.

top(Grid, NumOfColumns, Index, IndexRes):-
	IndexT is Index-NumOfColumns,
	nth0(Index, Grid, Elem),
	nth0(IndexT, Grid, Top),
	Elem =:= Top,
	IndexRes = IndexT.

topRight(Grid, NumOfColumns, Index, IndexRes):-
	IndexTR is Index-NumOfColumns+1,
	nth0(Index, Grid, Elem),
	nth0(IndexTR, Grid, TR),
	Elem =:= TR,
	IndexRes = IndexTR.

left(Grid, _, Index, IndexRes):-
	IndexL is Index-1,
	nth0(Index, Grid, Elem),
	nth0(IndexL, Grid, Left),
	Elem =:= Left,
	IndexRes = IndexL.

right(Grid, _, Index, IndexRes):-
	IndexR is Index+1,
	nth0(Index, Grid, Elem),
	nth0(IndexR, Grid, Right),
	Elem =:= Right,
	IndexRes = IndexR.

bottomLeft(Grid, NumOfColumns, Index, IndexRes):-
	IndexBL is Index+NumOfColumns-1,
	nth0(Index, Grid, Elem),
	nth0(IndexBL, Grid, BL),
	Elem =:= BL,
	IndexRes = IndexBL.

bottom(Grid, NumOfColumns, Index, IndexRes):-
	IndexB is Index+NumOfColumns,
	nth0(Index, Grid, Elem),
	nth0(IndexB, Grid, Bottom),
	Elem =:= Bottom,
	IndexRes = IndexB.

bottomRight(Grid, NumOfColumns, Index, IndexRes):-
	IndexBR is Index+NumOfColumns+1,
	nth0(Index, Grid, Elem),
	nth0(IndexBR, Grid, BR),
	Elem =:= BR,	
	IndexRes = IndexBR.