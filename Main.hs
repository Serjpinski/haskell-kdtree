module Main where

import Position
import BinaryTree
import KDTree

main = putStr (show (nearest (addList create [MakePosition 5 5, MakePosition 0 1, MakePosition 6 0, MakePosition 2 0, MakePosition 4 5, MakePosition 6 (-1)]) (MakePosition 4.1 0)))

--main = pict (addList create [MakePosition 5 5, MakePosition 0 1, MakePosition 6 0, MakePosition 2 0, MakePosition 4 5, MakePosition 6 (-1)])
