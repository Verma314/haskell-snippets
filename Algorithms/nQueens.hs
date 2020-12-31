import qualified Data.Map as Map

data SquareProperty = Queen | Free | Dead deriving (Show)
data Square = Square SquareProperty deriving (Show)

type BoxNumber = (Int,Int)
type ChessBoard =  [ (BoxNumber,Square)]


myChessBoxes :: [BoxNumber]
myChessBoxes = cartProd [0..7] [0..7] -- data member 


constructChessBoard :: [BoxNumber] -> ChessBoard
constructChessBoard chessBoxes = map (\ box -> (box, Square Free))  chessBoxes

myChessBoard = constructChessBoard myChessBoxes  --data member 

getMapOfChessBoard ::  ChessBoard -> Map.Map BoxNumber Square 
getMapOfChessBoard chessBoard = Map.fromList chessBoard


myChessBoardMap = getMapOfChessBoard myChessBoard -- data member



markBoxAsQueen :: ChessBoard -> BoxNumber -> ChessBoard
markBoxAsQueen chessBoard boxCoordinates =  Map.toList modifiedMap
                                           where initialMap = getMapOfChessBoard chessBoard
                                                 modifiedMap =  Map.insert boxCoordinates (Square Queen) initialMap


markBoxAsDead :: ChessBoard -> BoxNumber -> ChessBoard
markBoxAsDead chessBoard boxCoordinates =  Map.toList modifiedMap
                                           where initialMap = getMapOfChessBoard chessBoard
                                                 modifiedMap =  Map.insert boxCoordinates (Square Dead) initialMap


markListAsDead :: ChessBoard -> [BoxNumber] -> ChessBoard
markListAsDead chessBoard list = foldl markBoxAsDead chessBoard list


{-

markBoxAsDead   markBoxAsQueen  markListAsDead
*Main Data.Maybe Map> markListAsDead myChessBoard [(0,0),(0,1),(0,2)]
[((0,0),Square Dead),((0,1),Square Dead),((0,2),Square Dead),((0,3),Square Free),((0,4),Square Free),((0,5),Square Free),((0,6),Square Free),((0,7),Square Free),((1,0),Square Free),((1,1),Square Free),((1,2),Square Free),((1,3),Square Free),((1,4),Square Free),((1,5),Square Free),((1,6),Square Free),((1,7),Square Free),((2,0),Square Free),((2,1),Square Free),((2,2),Square Free),((2,3),Square Free),((2,4),Square Free),((2,5),Square Free),((2,6),Square Free),((2,7),Square Free),((3,0),Square Free),((3,1),Square Free),((3,2),Square Free),((3,3),Square Free),((3,4),Square Free),((3,5),Square Free),((3,6),Square Free),((3,7),Square Free),((4,0),Square Free),((4,1),Square Free),((4,2),Square Free),((4,3),Square Free),((4,4),Square Free),((4,5),Square Free),((4,6),Square Free),((4,7),Square Free),((5,0),Square Free),((5,1),Square Free),((5,2),Square Free),((5,3),Square Free),((5,4),Square Free),((5,5),Square Free),((5,6),Square Free),((5,7),Square Free),((6,0),Square Free),((6,1),Square Free),((6,2),Square Free),((6,3),Square Free),((6,4),Square Free),((6,5),Square Free),((6,6),Square Free),((6,7),Square Free),((7,0),Square Free),((7,1),Square Free),((7,2),Square Free),((7,3),Square Free),((7,4),Square Free),((7,5),Square Free),((7,6),Square Free),((7,7),Square Free)]

So far, works!

-}

-- todo: to create queen's hit list
-- given a Coordinate, return the list of coordinates to convert to "Dead"








------------ helper functions:

cartProd xs ys = [(x,y) | x <- xs, y <- ys]