module BinaryTree where

data Tree a		= Null | Node a (Tree a) (Tree a) deriving (Eq,Show)

leaf a 		= Node a Null Null

-- Tamaño del arbol
size Null = 0
size (Node _ tl tr) = 1 + size tl + size tr

-- Convierte el arbol en una lista
treeToList Null = []
treeToList (Node x xl xr) = x : treeToList xl ++ treeToList xr

-- Imprime un arbol por pantalla, dandole cierto formato.
pict t = putStr (pic "" t)
         where pic ind Null = ind ++ "."
               pic ind (Node x Null Null) = ind ++ show x
               pic ind (Node x Null tr) = pic ('\t':ind) tr ++ "\n" ++
                                        ind ++ show x
               pic ind (Node x tl Null) = ind ++ show x     ++ "\n" ++
                                        pic ('\t':ind) tl
               pic ind (Node x tl tr) = pic ('\t':ind) tr ++ "\n" ++
                                        ind ++ show x     ++ "\n" ++
                                        pic ('\t':ind) tl

-- Encuentra la hoja mas lejana por la izquierda
farLft (Node x Null _) = x
farLft (Node _ xl _) = farLft xl

-- Encuentra la hoja mas lejana por la derecha
farRt (Node x _ Null) = x
farRt (Node _ _ xr) = farRt xr




