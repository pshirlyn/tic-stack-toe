import Data.List
import System.IO
import Control.Monad.State.Lazy

import Control.Monad.IO.Class
import Data.IORef

import qualified Data.Text as T
import qualified GI.Gtk.Functions as GI (main, init)
import GI.Gtk hiding (get, Action, main)

data UIState = UIState { active    :: Square
                       , board     :: Board
                       , gameState :: GameState
                       } deriving (Show)

data Square = X | O | Empty
    deriving (Eq)
instance Show Square where
    show X = "X"
    show O = "O"
    show Empty = "_"

type Board = [[Square]]

data Action = Quit | Play Int Int
    deriving (Show)

data GameState = Won | Tied | Playing deriving (Show)

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
    where diagonals [[a,b,c],
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

oneMove :: Board
oneMove = evalState (move 0 0 X) emptyBoard

-- returns a new List replacing element at index with given elem
replace :: Int -> a -> [a] -> [a]
replace index elem = map (\(index', elem') -> if index' == index then elem else elem') . zip [0..]

-- get the inverse of a player, accepts only X and O
invertPlayer :: Square -> Square
invertPlayer player
    | player == X = O
    | player == O = X
    | otherwise   = error "undefined player"

parseAction :: String -> Action
parseAction input = 
    if input == "q"
    then Quit
    else let num = read input in
         let (x, y) = num `divMod` 3 in
         Play x y

boardState :: Board -> GameState
boardState board =
    if won board
    then Won
    else if tied board
        then Tied
        else Playing

play :: Square -> StateT Board IO ()
play player = do
    lift $ putStrLn $ "Enter move, Player " ++ show player
    square <- lift getLine
    case parseAction square of
      Quit     -> lift $ putStrLn $ "Exiting"
      Play x y -> do
        board <- (state . runState) $ move x y player -- hoisting
        lift $ print $ board
        case boardState board of
          Won     -> lift $ putStrLn $ show player ++ " won!"
          Tied    -> lift $ putStrLn "game tied"
          Playing -> play $ invertPlayer player


textGame :: IO Board
textGame = execStateT (play X) emptyBoard

-- get a string representation of the square at this position
getRow :: Board -> Int -> String
getRow board x = show (board !! x)

-- renderBoard :: Board -> IO ()
-- renderBoard board = 

getSquare :: Board -> Int -> Int -> String
getSquare board x y 
    | square == Empty = "_"
    | square == X     = "X"
    | square == O     = "O" 
    where square = (board !! x) !! y

mkButton
    :: IORef UIState
    -> (UIState -> UIState)
    -> IO Button
mkButton st mutateState = do
    btn <- buttonNew
    set btn [ buttonLabel := T.pack "_" ]
    onButtonClicked btn $ do
        newState <- atomicModifyIORef st $ \x -> let r = mutateState x in (r, r)
        set btn [ buttonLabel := T.pack (show $ active newState) ]
    return btn

main :: IO ()
main = do
    st <- newIORef (UIState X emptyBoard Playing)
    GI.init Nothing
    window <- windowNew WindowTypeToplevel
    set window [ windowTitle         := T.pack "Tic-Tac-Toe"
                , windowResizable    := True
                , windowDefaultWidth  := 600
                , windowDefaultHeight := 300 ]
    
    grid <- gridNew
    gridSetRowHomogeneous grid True

    let attach x y w h item = gridAttach grid item x y w h
        mkBtn = mkButton st id
    mkBtn  >>= attach 0 0 1 1
    mkBtn  >>= attach 0 1 1 1
    mkBtn  >>= attach 0 2 1 1
    mkBtn  >>= attach 1 0 1 1
    mkBtn  >>= attach 1 1 1 1
    mkBtn  >>= attach 1 2 1 1
    mkBtn  >>= attach 2 0 1 1
    mkBtn  >>= attach 2 1 1 1
    mkBtn  >>= attach 2 2 1 1    
    containerAdd window grid

    onWidgetDestroy window mainQuit
    widgetShowAll window
    GI.main  