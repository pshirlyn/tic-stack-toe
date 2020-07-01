{-# LANGUAGE BlockArguments #-}

import Data.List
import System.IO
import Control.Monad.State.Lazy

import Control.Monad.IO.Class
import Data.IORef

import qualified Data.Text as T
import qualified GI.Gtk.Functions as GI (main, init)
import GI.Gtk hiding (get, Action, main)

-- used to display the game and compute next state
data UIState = UIState { active    :: Player
                       , board     :: Board
                       } deriving (Show)

data Square = X | O | Empty
    deriving (Eq)
instance Show Square where
    show X = "X"
    show O = "O"
    show Empty = "_"

data Player = PlayerX | PlayerO deriving (Show, Eq)

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

-- place given square on (x, y) in board state
move :: Int -> Int -> Square -> State Board Board
move x y square = do
    board <- get
    let board' = replace x (replace y square (board !! x)) board
    put board'
    return board'

-- returns a new List replacing element at index with given elem
replace :: Int -> a -> [a] -> [a]
replace index elem = map (\(index', elem') -> if index' == index then elem else elem') . zip [0..]

-- get the inverse of a player, accepts only X and O
invertPlayer :: Player -> Player
invertPlayer player
    | player == PlayerX = PlayerO
    | player == PlayerO = PlayerX

getPlayerSquare :: Player -> Square
getPlayerSquare player
    | player == PlayerX = X
    | player == PlayerO = O

-- get a string representation of the square at this position
getRow :: Board -> Int -> String
getRow board x = show (board !! x)

getSquare :: Board -> Int -> Int -> String
getSquare board x y 
    | square == Empty = "_"
    | square == X     = "X"
    | square == O     = "O"
    where square = (board !! x) !! y

-- determine user input at beginning of game loop
parseAction :: String -> Action
parseAction input = 
    if input == "q"
    then Quit
    else let num = read input in
         let (x, y) = num `divMod` 3 in
         Play x y

-- calculate win condition, if any, given board
boardState :: Board -> GameState
boardState board =
    if won board
    then Won
    else if tied board
        then Tied
        else Playing

-- run game loop given a player X or O
play :: Player -> StateT Board IO ()
play player = do
    lift $ putStrLn $ "Enter move, Player " ++ show player
    square <- lift getLine
    case parseAction square of
      Quit     -> lift $ putStrLn $ "Exiting"
      Play x y -> do
        board <- (state . runState) $ move x y (getPlayerSquare player)
        lift $ print $ board
        case boardState board of
          Won     -> lift $ putStrLn $ show player ++ " won!"
          Tied    -> lift $ putStrLn "game tied"
          Playing -> play $ invertPlayer player

-- play game through interactive shell
textGame :: IO Board
textGame = execStateT (play PlayerX) emptyBoard

-- place square on (x, y)
makeMove :: Int -> Int -> Square -> Board -> Board
makeMove x y square board = replace x (replace y square (board !! x)) board

-- text displayed at game end
gameState :: UIState -> GameState
gameState st = boardState $ board st

mkButton
    :: IORef UIState
    -> Entry
    -> (UIState -> UIState)
    -> IO Button
mkButton currSt entry gameFunction = do
    btn <- buttonNew
    set btn [ buttonLabel := T.pack "_" ]
    onButtonClicked btn $ do
        newState <- atomicModifyIORef currSt $ \x -> let r = gameFunction x in (r, r)
        set btn [ buttonLabel := (T.pack . show . getPlayerSquare . active) newState]
        set entry [ entryText := (T.pack . show . gameState) newState]
    return btn

main :: IO ()
main = do
    st <- newIORef (UIState PlayerO emptyBoard)
    GI.init Nothing
    window <- windowNew WindowTypeToplevel
    set window [ windowTitle         := T.pack "Tic-Tac-Toe"
                , windowResizable    := True
                , windowDefaultWidth  := 600
                , windowDefaultHeight := 300 ]
    
    grid <- gridNew
    gridSetRowHomogeneous grid True

    display <- entryNew
    set display [ entryEditable := False
                , entryText     := T.pack "Playing" ]
    
    gridAttach grid display 0 3 3 1

    let attach x y item = gridAttach grid item x y 1 1
        mkBtn x y = mkButton st display (handleClick x y) 
        putBtn x y = mkBtn x y >>= attach (fromIntegral x) (fromIntegral y)
    
    forM [0..2] \col ->
        forM [0..2] \row ->
            putBtn row col
    containerAdd window grid

    onWidgetDestroy window mainQuit
    widgetShowAll window
    GI.main

handleClick :: Int -> Int ->  UIState -> UIState
handleClick x y currentState =
    UIState { active = currentPlayer
            , board = newBoard
            }
    where currentPlayer = invertPlayer $ active currentState
          newBoard      = makeMove x y (getPlayerSquare currentPlayer) $ board currentState