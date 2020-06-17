import Data.List
import System.IO
import Control.Monad.State.Lazy

import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (get, Action)

-- import Brick
-- import Brick.BChan (newBChan, writeBChan)
-- import qualified Brick.Main as M
-- import qualified Brick.Widgets.Border as B
-- import qualified Brick.Widgets.Border.Style as BS
-- import qualified Brick.Widgets.Center as C
-- import qualified Brick.Widgets.Core as CR
-- import qualified Graphics.Vty as V

data Square = X | O | Empty
    deriving (Show, Eq)
type Board = [[Square]]

data Action = Quit | Play Int Int
    deriving (Show)

data GameState = Won | Tied | Playing

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

-- app :: App Board
-- app = App { appDraw = drawUI
--             , appChooseCursor = 
-- }

-- renderUI :: Board -> [Widget ()]
-- renderUI b = [renderBoard b]

-- renderBoard :: Board -> Widget ()
-- renderBoard b = 
--             CR.joinBorders $
--             CR.hLimit 6 $
--             CR.vLimit 7 $
--             displayRow b 0 
--             <=> B.hBorder
--             <=> displayRow b 1 
--             <=> B.hBorder
--             <=> displayRow b 2

-- displayRow :: Board -> Int -> Widget ()
-- displayRow board i = str (getSquare board i 0)
--                 <+>  B.vBorder
--                 <+>  str (getSquare board i 1)
--                 <+>  B.vBorder
--                 <+>  str (getSquare board i 2)
--         where display = B.border . str

-- appEvent :: Board -> BrickEvent n e -> EventM n (Next Board)
-- appEvent 

-- attMap :: Brick.AttrMap
-- attMap = Brick.attrMap V.defAttr []

-- main :: IO ()
-- main = do
--     let app = M.App { M.appDraw = renderUI
--           , M.appChooseCursor = M.showFirstCursor
--           , M.appHandleEvent = appEvent
--           , M.appStartEvent = return
--           , M.appAttrMap = const attMap
--           }
--     finalState <- defaultMain app emptyBoard
--     putStr $ show finalState


    -- void $ defaultMain app emptyBoard
    -- simpleMain $ renderBoard emptyBoard

getSquare :: Board -> Int -> Int -> String
getSquare board x y 
    | square == Empty = "_"
    | square == X     = "X"
    | square == O     = "O" 
    where square = (board !! x) !! y

main :: IO ()
main = do
    void initGUI
    window <- windowNew
    set window [ windowTitle         := "Tic-Tac-Toe"
                , windowResizable    := True
                , windowDefaultWidth  := 600
                , windowDefaultHeight := 600 ]
    
    grid <- gridNew
    -- gridSetRowHomogeneous grid True

    -- let attach x y w h item = gridAttach grid item x y w h
    -- mkBtn "_"   >>= attach 0 0 1 1
    -- mkBtn "_"   >>= attach 0 1 1 1
    -- mkBtn "_"   >>= attach 0 2 1 1
    -- mkBtn "_"   >>= attach 1 0 1 1
    -- mkBtn "_"   >>= attach 1 1 1 1
    -- mkBtn "_"   >>= attach 1 2 1 1
    -- mkBtn "_"   >>= attach 2 0 1 1
    -- mkBtn "_"   >>= attach 2 1 1 1
    -- mkBtn "_"   >>= attach 2 2 1 1    
    -- containerAdd window grid

    widgetShowAll window
    mainGUI  