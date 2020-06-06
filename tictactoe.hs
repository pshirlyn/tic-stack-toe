import Data.List
import System.IO
import Control.Monad.State.Lazy

data Square = X | O | Empty
    deriving (Show, Eq)
type Board = [[Square]]

-- generate an empty board
emptyBoard :: Board
emptyBoard = [[Empty, Empty, Empty],
              [Empty, Empty, Empty],
              [Empty, Empty, Empty]]

-- check if board is a tie
tied :: Board -> Bool
tied board = not (won board || any (== Empty) (concat board))

-- check if any win condition has been reached
won :: Board -> Bool
won board = any allSame (board ++ transpose board ++ diagonals board)

-- get the diagonals of this board
diagonals :: Board -> [[Square]]
diagonals [[a,b,c],
            [d,e,f],
            [g,h,i]] = [[a,e,i],[c,e,g]]

-- check if every value in this list has the same value, not Empty
allSame :: [Square] -> Bool
allSame [] = True
allSame (Empty:_) = False
allSame (x:xs) = all (== x) xs

-- make a move for given player
move :: Int -> Int -> Square -> State Board Board
move x y square = do
    board <- get
    let board' = replace x (replace y square (board !! x)) board
    put board'
    return board'

replace :: Int -> a -> [a] -> [a]
replace index elem = map (\(index', elem') -> if index' == index then elem else elem') . zip [0..]

invertPlayer :: Square -> Square
invertPlayer player
    | player == X = O
    | player == O = X
    | otherwise   = error "undefined player"

-- play the game
play :: Square -> IO()
play player = do
    putStrLn $ "Enter move, Player " ++ show player
    square <- getLine
    let (x, y) = square `divMod` 3
    board <- move x y player
    print $ board
    play $ invertPlayer player

testOut :: IO()
testOut = do
    print $ evalState testSeq emptyBoard
    where testSeq = do
            board <- get
            board' <- move 1 2 X
            boardState <- move 1 1 O
            return boardState