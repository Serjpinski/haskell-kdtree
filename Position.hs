module Position where

data Axis		= X | Y deriving (Eq)
instance Show Axis where
	show X = "X"
	show Y = "Y"

class HasPos a where
-- Clase para los tipos que contienen una posicion
	getPos :: a -> Position
	-- devuelve la posicion

data Position 		= MakePosition Float Float deriving (Eq)
positionx                  :: Position -> Float
positionx (MakePosition x _)  = x
positiony                  :: Position -> Float
positiony (MakePosition _ y)  = y
positionaxis X p		= positionx p
positionaxis Y p		= positiony p

instance Show Position where
	show (MakePosition x y) = "(" ++ show x ++ " " ++ show y ++ ")"

instance HasPos Position where
-- el tipo Position deriva la clase HasPos
	getPos p = p

distance :: Position -> Position -> Float
-- calucla la distancia entre dos posiciones
distance (MakePosition x1 y1) (MakePosition x2 y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)
