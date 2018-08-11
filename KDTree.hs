module KDTree where

import Position
import BinaryTree

data Direction = L | R deriving (Eq)

create :: (HasPos a) => Tree a
-- crea un arbol vacio
create = Null

add :: (HasPos a) => Tree a -> a -> Tree a
-- anade una posicion a un arbol
add t p = add_Explore t p X
--  inicializa el eje a X
	where

	add_Explore :: (HasPos a) => Tree a -> a -> Axis -> Tree a
	-- explora el arbol descendiendo hasta llegar a una hoja, e inserta alli el nuevo nodo
	add_Explore Null p ax = Node p Null Null
	-- inserta el nodo
	add_Explore (Node p0 tl tr) p ax =
	-- explora
		if ax == X
		-- verifica el eje
			then
				if positionx (getPos p) < positionx (getPos p0)
				-- compara las posiciones del eje actual
					then Node p0 (add_Explore tl p Y) tr
					-- desciende por la izquierda cambiando el eje
					else Node p0 tl (add_Explore tr p Y)
					-- desciende por la derecha cambiando el eje
			else
			--simetrico (ahora compara segun el otro eje)
				if positiony (getPos p) < positiony (getPos p0)
					then Node p0 (add_Explore tl p X) tr
					else Node p0 tl (add_Explore tr p X)

addList :: (HasPos a) => Tree a -> [a] -> Tree a
-- anade una lista de posiciones a un arbol
addList t [] = t
-- si la lista esta vacia, no cambia el arbol
addList t (p:ps) = addList (add t p) ps
-- anade el primero de la lista y hace recursion con el resto

nearest :: (HasPos a) => Tree a -> a -> a
-- devuelve la posicion mas cercana a una dada
nearest Null p = error "Error: arbol vacio."
-- si el arbol es vacio, devuelve un error
nearest (Node p0 tl tr) p = nearest_Explore (Node p0 tl tr) p X p0
-- inicializa el eje a X y la posicion candidata a la de la raíz
	where

	nearest_Explore :: (HasPos a) => Tree a -> a -> Axis -> a -> a
	-- explora el arbol buscando posiciones mas cercanas que la candidata
	nearest_Explore Null p ax near = near
	-- si el nodo es vacio, no cambia la posicion candidata
	nearest_Explore (Node p0 Null Null) p ax near =
	-- si el nodo es una hoja, cambia la posicion candidata si procede
		if distance (getPos p) (getPos p0) < distance (getPos p) (getPos near)
			then p0
			else near
	nearest_Explore (Node p0 tl tr) p ax near =
	-- si el nodo tiene hijos, los explora y cambia la posicion candidata si procede
		if ax == X
		-- verifica el eje
			then
				if positionx (getPos p) < positionx (getPos p0)
				-- compara las posiciones del eje actual
					then
						if distance (getPos p) (getPos p0) < distance (getPos p) (getPos near)
						-- compara las posiciones actual y candidata
							then nearest_CheckSubtree (Node p0 tl tr) p ax R (nearest_Explore tl p Y p0)
							-- desciende por el subarbol izquierdo y luego chequea el derecho,
							-- cambiando la posicion candidata a la actual
							-- al salir chequea el subarbol opuesto
							else nearest_CheckSubtree (Node p0 tl tr) p ax R (nearest_Explore tl p Y near)
							-- lo mismo, pero conservando la posicion candidata
					else
					-- simetrico (ahora desciende por la derecha)
						if distance (getPos p) (getPos p0) < distance (getPos p) (getPos near)
							then nearest_CheckSubtree (Node p0 tl tr) p ax L (nearest_Explore tr p Y p0)
							else nearest_CheckSubtree (Node p0 tl tr) p ax L (nearest_Explore tr p Y near)
			else
			-- simetrico (ahora compara segun el otro eje)
				if positiony (getPos p) < positiony (getPos p0)
					then
						if (distance (getPos p) (getPos p0)) < distance (getPos p) (getPos near)
							then nearest_CheckSubtree (Node p0 tl tr) p ax R (nearest_Explore tl p X p0)
							else nearest_CheckSubtree (Node p0 tl tr) p ax R (nearest_Explore tl p X near)
					else
						if distance (getPos p) (getPos p0) < distance (getPos p) (getPos near)
							then nearest_CheckSubtree (Node p0 tl tr) p ax L (nearest_Explore tr p X p0)
							else nearest_CheckSubtree (Node p0 tl tr) p ax L (nearest_Explore tr p X near)

	nearest_CheckSubtree :: (HasPos a) => Tree a -> a -> Axis -> Direction -> a -> a
	-- chequea el subarbol indicado para comprobar si puede contener posiciones mas cercanas que la candidata
	nearest_CheckSubtree (Node p0 tl tr) p ax dir near =
		if ax == X
		-- verifica el eje
			then
				if abs (positionx (getPos p) - positionx (getPos p0)) < distance (getPos p) (getPos near)
				-- verifica si los hijos del nodo pueden contener candidatos
					then
						if dir == L
						-- verifica el subarbol
							then nearest_Explore tl p Y near
							-- explora el subarbol elegido
							else nearest_Explore tr p Y near
					else near
					-- conserva la posicion candidata
			else
			-- simetrico (ahora compara segun el otro eje)
				if abs (positiony (getPos p) - positiony (getPos p0)) < distance (getPos p) (getPos near)
					then
						if dir == L
							then nearest_Explore tl p X near
							else nearest_Explore tr p X near
					else near
