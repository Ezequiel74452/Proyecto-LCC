:- module(proylcc, 
	[  
		join/4
	]).

/*
	join(Grid, NumOfColumns, Path, RGrids) 
	RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
	en la grilla Grid, con número de columnas NumOfColumns.
	Si se llama a join con un Path vacío es porque se usó el power-up, así que se llama a booster/6 el cual junta todos los grupos de bloques de igual valor.
	Si el Path posee elementos, entonces join llama a removePath/5 que se encarga de vaciar el camino, hacer el efecto de gravedad y añadir los nuevos bloques.
	El número 0 representa que la celda está vacía.
*/ 
join(Grid, NumOfColumns, [], RGrids):-
	booster(Grid, NumOfColumns, 0, [], RGridsAux, []),
	RGrids = RGridsAux.

join(Grid, NumOfColumns, Path, RGrids):-
	removePath(Grid, NumOfColumns, Path, RGrids, 0).

/*
	removePath(Grid, NumOfColumns, Path, RGrids, Sum):
	Caso base: cuando el camino está vacío porque significa que ya se eliminaron todos los bloques del mismo.
	Llama a bajar/4 que se encarga de hacer el efecto de gravedad y a  insertarNuevos/3 que genera los nuevos
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

/*
	insertarNuevos(Grid, Index, Res):
	Caso final: cuando llego al final de la grilla (Index = 40), ya no hay más bloques que insertar.
*/
insertarNuevos(Grid, 40, Res):-
	Res = Grid.

/*
	insertarNuevos(Grid, Index, Res):
	Caso recursivo 1:
		Si el índice que estoy mirando es 0, entonces busca un random entre 1 y 10, luego crea un nuevo
		bloque con valor 2 elevado al número generado y finalmente llama recursivamente con el próximo índice.
*/
insertarNuevos(Grid, Index, Res):-
	nth0(Index, Grid, 0),
	random(1, 10, Aux),
	pow(2, Aux, Num),
	setByIndex(Grid, Index, Num, GridN),
	Indexaux is Index+1,
	insertarNuevos(GridN, Indexaux, Res).

/*
	insertarNuevos(Grid, Index, Res):
	Caso recursivo 2:
		Saltea y llama recursivamente con el próximo índice.
*/
insertarNuevos(Grid, Index, Res):-
	Indexaux is Index+1,
	insertarNuevos(Grid, Indexaux, Res).

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
	booster(Grid, NumOfColumns, Index, NoVisit, GridR, List):
	Caso final: cuando se llega al índice 40 porque ya se recorrió toda la grilla.
		Llama a insertList/3 para insertar en la grilla los bloques almacenados en List, según
		su índice. Luego, baja los ceros y añade los nuevos bloques
*/
booster(Grid, NumOfColumns, 40, _, [Res, Xs, Ys], List):-
	insertList(Grid, List, Res),
	bajar(Res, NumOfColumns, 0, Xs),
	insertarNuevos(Xs, 0, Ys).

/*
	booster(Grid, NumOfColumns, Index, Visited, GridR, List):
	Recibe una grilla (Grid), su cantidad de columnas (NumOfColumns), un índice (Index), una lista de
	posiciones visitadas (Visited) o más precisamente, de elementos que no hay que visitar porque ya están
	capturados en un grupo y una lista de listas (List) de la forma [[I, V], [I2, V2], ..., [IN, VN]] donde
	cada IN representa el índice de la última posición de un grupo de bloques iguales y V el valor que se le asignará a ese índice en la grilla.
	Devuelve la grilla resultante de aplicar el efecto del booster "colapsar iguales".
	Algoritmo:
		- Toma la lista de adyacentes iguales al índice actual y a partir de eso empieza a calcular
		todos los demás adyacentes iguales hasta tener el grupo entero.
		- Toma la cantidad de adyacentes para usarlos en borrar/6.
		- Elimino de la grilla todos los bloques del grupo actual.
		- Junto las listas de posiciones visitadas y la ordeno.
		- Pasa al siguiente índice, si no fue visitado llama recursivamente, caso contrario llama a boosterAux/6.
*/
booster(Grid, NumOfColumns, Index, Visited, GridR, List):-
	getAdjacent(Grid, NumOfColumns, Index, VisitedAux),
	length(VisitedAux, L),
	borrar(Grid, VisitedAux, L, Grid1, List, ListR),
	append(Visited, VisitedAux, Res),
	sort(Res, Sorted),
	Indexaux is Index+1,
	\+ member(Indexaux, Sorted),
	booster(Grid1, NumOfColumns, Indexaux, Sorted, GridR, ListR);
	getAdjacent(Grid, NumOfColumns, Index, VisitedAux),
	length(VisitedAux, L),
	borrar(Grid, VisitedAux, L, Grid1, List, ListR),
	append(Visited, VisitedAux, Res),
	sort(Res, Sorted),
	Indexaux is Index+1,
	boosterAux(Grid1, NumOfColumns, Indexaux, Sorted, GridR, ListR).

/*
	boosterAux(Grid, NumOfColumns, Index, Visited, GridR, List):
	Igual al caso final de booster/6.
*/
boosterAux(Grid, NumOfColumns, 40, _, [Res, Xs, Ys], List):-
	insertList(Grid, List, Res),
	bajar(Res, NumOfColumns, 0, Xs),
	insertarNuevos(Xs, 0, Ys).

/*
	boosterAux(Grid, NumOfColumns, Index, Visited, GridR, List):
	Si se llama a boosterAux/6 es porque el índice pasado pertenece a la lista de visitados,
	por lo tanto, pasa al siguiente índice y vuelve a comprobar si este pertenece o no a la lista,
	llamando recursivamente a booster/6 o boosterAux/6, según corresponda.
*/
boosterAux(Grid, NumOfColumns, Index, Visited, GridR, List):-
	Indexaux is Index+1,
	\+ member(Indexaux, Visited),
	booster(Grid, NumOfColumns, Indexaux, Visited, GridR, List);
	Indexaux is Index+1,
	boosterAux(Grid, NumOfColumns, Indexaux, Visited, GridR, List).

/*
	borrar(Grid, Indexes, Tam, Res, List, ListR):
	Caso base: el grupo es vacío (el primer elemento no tenía adyacentes), no hay que borrar nada.
*/
borrar(Grid, [], _, Grid, List, List).

/*
	borrar(Grid, Indexes, Tam, Res, List, ListR):
	Caso final: X es el último elemento del grupo.
		Algoritmo:
		- Toma el valor del índice X en la grilla y lo multiplica por el tamaño del grupo.
		- Calcula la menor potencia de 2 mayor o igual.
		- Añade el par [Índice, Valor] a la lista de pares índice/valor.
		- Elimina el elemento en la grilla.
*/
borrar(Grid, [X|Ys], Tam, Res, List, ListR):-
	isEmpty(Ys),
	nth0(X, Grid, Aux),
	Sumaux is Aux*Tam,
	nextPower(Sumaux, 2, Pot),
	concatenarListas([X, Pot], List, ListR),
	setByIndex(Grid, X, 0, Res).

/*
	borrar(Grid, Indexes, Tam, Res, List, ListR):
	Recibe una Grilla (Grid), una lista de índices adyacentes e iguales(Indexes), el tamaño de dicha
	lista (Tam) y una lista de listas índice/valor [[I1, V1], [I2, V2], ..., [IN, VN]] (List) y retorna
	la grilla resultante de borrar todos los índices en Indexes y una nueva lista de índice/valor 
	añadiéndole el índice y el valor que va a tomar el bloque que se generará en la última posición del grupo. 
	Caso recursivo: hay por lo menos dos elementos en el grupo.
		Elimina el que está evaluando actualmente y pasa al siguiente.
*/
borrar(Grid, [X|Ys], Tam, Res, List, ListR):-
	setByIndex(Grid, X, 0, GridAux),
	borrar(GridAux, Ys, Tam, Res, List, ListR).

/*
	insertList(Grid, L, Res):
	Caso base: cuando la lista de índices/valores está vacía, es decir, ya se insertó todo.
*/
insertList(Grid, [], Grid).

/*
	insertList(Grid, L, Res):
	Dada una lista de listas de dos elementos I = índice, V = valor, inserta en Grid todos los valors VN
	en su respectivo índice IN.
	Caso recursivo: si hay 1 o más listas de índice/valor para insertar en la grilla
*/
insertList(Grid, [[I,V]|Xs], Res):-
	setByIndex(Grid, I, V, Aux),
	insertList(Aux, Xs, Res).

/*
	getAdjacent(Grid, NumOfColumns, Index, AdjacentListSorted):
	Recibe la grilla (Grid), su número de columnas y un índice.
	Devuelve una lista con los adyacentes con igual valor al índice dado, los adyacented de estos
	y así hasta obtener el grupo de iguales adyacentes entero.
*/
getAdjacent(Grid, NumOfColumns, Index, AdjacentListSorted):-
	tieneAdy(Grid, NumOfColumns, Index, AdjacentIndexes),
	getAdjacentGroup(Grid, NumOfColumns, AdjacentIndexes, [], AdjacentList),
	sort(AdjacentList, AdjacentListSorted).

/*
	getAdjacentGroup(Grid, NumOfColumns, Indexes, Acc, AdjacentList):
	Caso final: cuando la lista de índices se acaba, ya no hay más índices en los cuales buscar más miembros
	del grupo, así que la lista con los índices del grupo es el acumulador.
*/
getAdjacentGroup(_, _, [], AdjacentList, AdjacentList).

/*
	getAdjacentGroup(Grid, NumOfColumns, Indexes, Acc, AdjacentList):
	Recibe la grilla Grid, su cantidad de columnas, una lista de índices pertenecientes al grupo (Indexes)
	y un acumulador (Acc), inicialmente vacío.
	Devuelve una lista con todos los índices del grupo de iguales.
	Caso recursivo 1: si Index está en el acumulador, ya lo miré, paso al siguiente índice.
*/
getAdjacentGroup(Grid, NumOfColumns, [Index|Rest], Acc, AdjacentList):-
	member(Index, Acc),
	getAdjacentGroup(Grid, NumOfColumns, Rest, Acc, AdjacentList).

/*
	getAdjacentGroup(Grid, NumOfColumns, Indexes, Acc, AdjacentList):
	Caso recursivo 2: si Index no está en el acumulador:
		- Le tomo los adyacentes.
		- Los junto con el resto de índices que faltan verificar
		- Añado el índice al acumulador.
*/
getAdjacentGroup(Grid, NumOfColumns, [Index|Rest], Acc, AdjacentList):-
	\+ member(Index, Acc),
	tieneAdy(Grid, NumOfColumns, Index, AdjacentIndexes),
	append(Rest, AdjacentIndexes, NewList),
	getAdjacentGroup(Grid, NumOfColumns, NewList, [Index|Acc], AdjacentList).

/*
	tieneAdy(Grid, NumOfColumns, 0, Res):
	Recibe la grilla, su número de columnas y un índice.
	Llama al predicado ady.../4 que corresponda según el índice.
	Devuelve todos los adyacentes cuyo bloque tengan el mismo valor en la grilla que Index. 
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
	Index<4,
	Index>0,
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

/*
	adyEsquina1(Grid, NumOfColumns, Index, Res):
	Recibe la grilla, su número de columnas y un índice.
	Si (Index = 0), entonces pertenece a la esquina arriba-izquierda y tiene adyacentes
	a derecha, abajo y abajo-derecha.
	Retorna todos los adyacentes cuyo bloque tengan el mismo valor en la grilla que Index.
*/
adyEsquina1(Grid, NumOfColumns, Index, Res):-
	findall(Ady, (right(Grid, NumOfColumns, Index, Ady);
					bottom(Grid, NumOfColumns, Index, Ady);
					bottomRight(Grid, NumOfColumns, Index, Ady)), Adyacentes),
	Res = Adyacentes.

/*
	adyEsquina2(Grid, NumOfColumns, Index, Res):
	Recibe la grilla, su número de columnas y un índice.
	Si (Index = 4), entonces pertenece a la esquina arriba-derecha y tiene adyacentes
	a izquierda, abajo-izquierda, y abajo.
	Retorna todos los adyacentes cuyo bloque tengan el mismo valor en la grilla que Index.
*/
adyEsquina2(Grid, NumOfColumns, Index, Res):-
	findall(Ady, (left(Grid, NumOfColumns, Index, Ady);
					bottom(Grid, NumOfColumns, Index, Ady);
					bottomLeft(Grid, NumOfColumns, Index, Ady)), Adyacentes),
	Res = Adyacentes.

/*
	adyEsquina3(Grid, NumOfColumns, Index, Res):
	Recibe la grilla, su número de columnas y un índice.
	Si (Index = 35), entonces pertenece a la esquina abajo-izquierda y tiene adyacentes
	arriba, arriba-derecha, y a derecha.
	Retorna todos los adyacentes cuyo bloque tengan el mismo valor en la grilla que Index.
*/
adyEsquina3(Grid, NumOfColumns, Index, Res):-
	findall(Ady, (top(Grid, NumOfColumns, Index, Ady);
					topRight(Grid, NumOfColumns, Index, Ady);
					right(Grid, NumOfColumns, Index, Ady)), Adyacentes),
	Res = Adyacentes.

/*
	adyEsquina4(Grid, NumOfColumns, Index, Res):
	Recibe la grilla, su número de columnas y un índice.
	Si (Index = 39), entonces pertenece a la esquina abajo-derecha y tiene adyacentes
	arriba-izquierda, arriba, y a izquierda.
	Retorna todos los adyacentes cuyo bloque tengan el mismo valor en la grilla que Index.
*/
adyEsquina4(Grid, NumOfColumns, Index, Res):-
	findall(Ady, (top(Grid, NumOfColumns, Index, Ady);
					topLeft(Grid, NumOfColumns, Index, Ady);
					left(Grid, NumOfColumns, Index, Ady)), Adyacentes),
	Res = Adyacentes.

/*
	adyPrimeraColumna(Grid, NumOfColumns, Index, Res):
	Recibe la grilla, su número de columnas y un índice.
	Si (Index mod NumOfColumns = 0), entonces pertenece a la primera columna y tiene adyacentes
	arriba, arriba-derecha, a derecha, abajo y abajo-derecha.
	Retorna todos los adyacentes cuyo bloque tengan el mismo valor en la grilla que Index.
*/
adyPrimeraColumna(Grid, NumOfColumns, Index, Res):-
	findall(Ady, (top(Grid, NumOfColumns, Index, Ady);
					topRight(Grid, NumOfColumns, Index, Ady);
					right(Grid, NumOfColumns, Index, Ady);
					bottom(Grid, NumOfColumns, Index, Ady);
					bottomRight(Grid, NumOfColumns, Index, Ady)), Adyacentes),
	Res = Adyacentes.

/*
	adyUltimaColumna(Grid, NumOfColumns, Index, Res):
	Recibe la grilla, su número de columnas y un índice.
	Si (Index mod NumOfColumns = 4), entonces pertenece a la última columna y tiene adyacentes
	arriba-izquierda, arriba, a izquierda, abajo-izquierda y abajo.
	Retorna todos los adyacentes cuyo bloque tengan el mismo valor en la grilla que Index.
*/
adyUltimaColumna(Grid, NumOfColumns, Index, Res):-
	findall(Ady, (top(Grid, NumOfColumns, Index, Ady);
					topLeft(Grid, NumOfColumns, Index, Ady);
					left(Grid, NumOfColumns, Index, Ady);
					bottom(Grid, NumOfColumns, Index, Ady);
					bottomLeft(Grid, NumOfColumns, Index, Ady)), Adyacentes),
	Res = Adyacentes.

/*
	adyPrimeraFila(Grid, NumOfColumns, Index, Res):
	Recibe la grilla, su número de columnas y un índice.
	Si el índice está entre 0 y 4, entonces pertenece a la primer fila y tiene adyacentes a
	izquierda, derecha, abajo-izquierda, abajo y abajo-derecha.
	Retorna todos los adyacentes cuyo bloque tengan el mismo valor en la grilla que Index.
*/
adyPrimeraFila(Grid, NumOfColumns, Index, Res):-
	findall(Ady, (left(Grid, NumOfColumns, Index, Ady);
					right(Grid, NumOfColumns, Index, Ady);
					bottomLeft(Grid, NumOfColumns, Index, Ady);
					bottom(Grid, NumOfColumns, Index, Ady);
					bottomRight(Grid, NumOfColumns, Index, Ady)), Adyacentes),
	Res = Adyacentes.

/*
	adyUltimaFila(Grid, NumOfColumns, Index, Res):
	Recibe la grilla, su número de columnas y un índice.
	Si el índice está entre 35 y 39, entonces pertenece a la última fila y tiene adyacentes
	arriba-izquierda, arriba, arriba-derecha, izquierda y derecha.
	Retorna todos los adyacentes cuyo bloque tengan el mismo valor en la grilla que Index.
*/
adyUltimaFila(Grid, NumOfColumns, Index, Res):-
	findall(Ady, (topLeft(Grid, NumOfColumns, Index, Ady);
					top(Grid, NumOfColumns, Index, Ady);
					topRight(Grid, NumOfColumns, Index, Ady);
					left(Grid, NumOfColumns, Index, Ady);
					right(Grid, NumOfColumns, Index, Ady)), Adyacentes),
	Res = Adyacentes.

/*
	adyNormal(Grid, NumOfColumns, Index, Res):
	Recibe la grilla, su número de columnas y un índice.
	Si el índice no pertenece ni a la primer fila, ni a la primer columna, ni a la última fila, ni a la
	última columna, entonces se considera un bloque normal y tiene adyacentes en todas las posiciones.
	Retorna todos los adyacentes cuyo bloque tengan el mismo valor en la grilla que Index.
*/
adyNormal(Grid, NumOfColumns, Index, Res):-
	findall(Ady, (topLeft(Grid, NumOfColumns, Index, Ady);
                    top(Grid, NumOfColumns, Index, Ady);
                    topRight(Grid, NumOfColumns, Index, Ady);
                    left(Grid, NumOfColumns, Index, Ady);
                    right(Grid, NumOfColumns, Index, Ady);
                    bottomLeft(Grid, NumOfColumns, Index, Ady);
                    bottom(Grid, NumOfColumns, Index, Ady);
                    bottomRight(Grid, NumOfColumns, Index, Ady)), Adyacentes),
    Res = Adyacentes.

/*
	topLeft(Grid, NumOfColumns, Index, IndexRes):
	Dada la grilla Grid, su número de columnas y un índice, devuelve el índice que está a la izquierda-arriba
	de Index si el valor de sus bloques es el mismo.
*/
topLeft(Grid, NumOfColumns, Index, IndexRes):-
	IndexTL is Index-NumOfColumns-1,
	nth0(Index, Grid, Elem),
	nth0(IndexTL, Grid, TL),
	Elem =:= TL,
	IndexRes = IndexTL.

/*
	top(Grid, NumOfColumns, Index, IndexRes):
	Dada la grilla Grid, su número de columnas y un índice, devuelve el índice que está arriba
	de Index si el valor de sus bloques es el mismo.
*/
top(Grid, NumOfColumns, Index, IndexRes):-
	IndexT is Index-NumOfColumns,
	nth0(Index, Grid, Elem),
	nth0(IndexT, Grid, Top),
	Elem =:= Top,
	IndexRes = IndexT.

/*
	topRight(Grid, NumOfColumns, Index, IndexRes):
	Dada la grilla Grid, su número de columnas y un índice, devuelve el índice que está a la derecha-arriba
	de Index si el valor de sus bloques es el mismo.
*/
topRight(Grid, NumOfColumns, Index, IndexRes):-
	IndexTR is Index-NumOfColumns+1,
	nth0(Index, Grid, Elem),
	nth0(IndexTR, Grid, TR),
	Elem =:= TR,
	IndexRes = IndexTR.

/*
	left(Grid, NumOfColumns, Index, IndexRes):
	Dada la grilla Grid y un índice, devuelve el índice que está a la izquierda de Index 
	si el valor de sus bloques es el mismo.
*/
left(Grid, _, Index, IndexRes):-
	IndexL is Index-1,
	nth0(Index, Grid, Elem),
	nth0(IndexL, Grid, Left),
	Elem =:= Left,
	IndexRes = IndexL.

/*
	right(Grid, NumOfColumns, Index, IndexRes):
	Dada la grilla Grid y un índice, devuelve el índice que está a la derecha de Index 
	si el valor de sus bloques es el mismo.
*/
right(Grid, _, Index, IndexRes):-
	IndexR is Index+1,
	nth0(Index, Grid, Elem),
	nth0(IndexR, Grid, Right),
	Elem =:= Right,
	IndexRes = IndexR.

/*
	bottomLeft(Grid, NumOfColumns, Index, IndexRes):
	Dada la grilla Grid, su número de columnas y un índice, devuelve el índice que está a la izquierda-abajo
	de Index si el valor de sus bloques es el mismo.
*/
bottomLeft(Grid, NumOfColumns, Index, IndexRes):-
	IndexBL is Index+NumOfColumns-1,
	nth0(Index, Grid, Elem),
	nth0(IndexBL, Grid, BL),
	Elem =:= BL,
	IndexRes = IndexBL.

/*
	bottom(Grid, NumOfColumns, Index, IndexRes):
	Dada la grilla Grid, su número de columnas y un índice, devuelve el índice que está abajo
	de Index si el valor de sus bloques es el mismo.
*/
bottom(Grid, NumOfColumns, Index, IndexRes):-
	IndexB is Index+NumOfColumns,
	nth0(Index, Grid, Elem),
	nth0(IndexB, Grid, Bottom),
	Elem =:= Bottom,
	IndexRes = IndexB.

/*
	bottomRight(Grid, NumOfColumns, Index, IndexRes):
	Dada la grilla Grid, su número de columnas y un índice, devuelve el índice que está a la derecha-abajo
	de Index si el valor de sus bloques es el mismo.
*/
bottomRight(Grid, NumOfColumns, Index, IndexRes):-
	IndexBR is Index+NumOfColumns+1,
	nth0(Index, Grid, Elem),
	nth0(IndexBR, Grid, BR),
	Elem =:= BR,	
	IndexRes = IndexBR.

/*
	setByIndex(Grid, Index, Num, Res):
	Busca el índice "Index" en la grilla y reemplaza su valor por el de "Num", luego retorna la nueva lista
	mediante Res.
*/
setByIndex(Grid, Index, Num, Res):-
	nth0(Index, Grid, _, Tail),
    nth0(Index, Res, Num, Tail).

/*
	isEmpty(Lista):
	Devuelve true si "Lista" está vacía.
*/
isEmpty([]).

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
	concatenarListas(L1, L2, L3):
	Caso L2 es vacía, entonces L3 = [L1].
*/
concatenarListas([I, V], [], [[I, V]]).

/*
	concatenarListas(L1, L2, L3):
	Dadas una lista L1 = [I, V] y una lista de listas de la forma L2= [[I1, V1], [I2, V2], ..., [IN, VN]],
	donde cada IN es un índice y VN el valor del bloque que va a ir en ese índice, se añaden ambas listas en
	L3, pero manteniéndose separadas del resto.
	Caso L2 no es vacío, entonces L3 = [L1|L2]
*/
concatenarListas([I, V], L, [[I, V]|L]):- 
	\+ isEmpty(L).

