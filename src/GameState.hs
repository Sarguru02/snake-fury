{-# Language RecordWildCards #-}
module GameState where

import           RenderState (BoardInfo (..), Point, {- DeltaBoard -})
import qualified RenderState as Board
import           Data.Sequence (Seq(..))
import qualified Data.Sequence as S
import           System.Random ({- uniformR, RandomGen(split), -} StdGen, Random (randomR))
-- import           Data.Maybe (isJust)

data Movement =
    North
  | South
  | East
  | West
  deriving (Show, Eq)

data SnakeSeq = SnakeSeq
  { snakeHead :: Point
  , snakeBody :: Seq Point
  }
  deriving (Show, Eq)

data GameState = GameState
  { snakeSeq :: SnakeSeq
  , applePosition :: Point
  , movement :: Movement
  , randomGen :: StdGen
  }
  deriving (Show, Eq)

oppositeMovement :: Movement -> Movement
oppositeMovement North = South
oppositeMovement South = North
oppositeMovement West  = East
oppositeMovement East  = West

makeRandomPoint :: BoardInfo -> StdGen -> (Point, StdGen)
makeRandomPoint BoardInfo{height=h, width=w} gen =
  let (randWidth, gen1) = randomR (1, w) gen
      (randHeight, gen2) = randomR (1, h) gen1
  in ((randHeight, randWidth), gen2)

inSnake :: Point -> SnakeSeq  -> Bool
inSnake p SnakeSeq{snakeHead=sh, snakeBody=sb} = p == sh || p `elem` sb

nextHead :: BoardInfo -> GameState -> Point
nextHead BoardInfo{height=h, width=w} GameState{movement=direction, snakeSeq=snake} =
  -- I probably shouldn't wrap. I should make the game over instead ?
  -- But for now let it be. The snake comes through the other side.
  let wrap1 n val = ((val-1) `mod` n) + 1 -- wraps from [1..n]
      wrap (x',y') = (wrap1 h x', wrap1 w y')
      (x,y) = snakeHead snake
  in wrap $ case direction of
    North -> (x-1, y)
    South -> (x+1, y)
    West  -> (x, y-1)
    East  -> (x, y+1)

newApple :: BoardInfo -> GameState -> (Point, StdGen)
newApple (BoardInfo h w) (GameState snake apple _ gen) =
  let points = [(i,j) | i <- [1..h], j <- [1..w], not (inSnake (i,j) snake), (i,j) /= apple]
      (idx, gen') = randomR (0, (length points)-1) gen
  in (points !! idx, gen')

-- | Moves the snake based on the current direction. It sends the adequate RenderMessage
-- Notice that a delta board must include all modified cells in the movement.
-- For example, if we move between this two steps
--        - - - -          - - - -
--        - 0 $ -    =>    - - 0 $
--        - - - -    =>    - - - -
--        - - - X          - - - X
-- We need to send the following delta: [((2,2), Empty), ((2,3), Snake), ((2,4), SnakeHead)]
--
-- Another example, if we move between this two steps
--        - - - -          - - - -
--        - - - -    =>    - X - -
--        - - - -    =>    - - - -
--        - 0 $ X          - 0 0 $
-- We need to send the following delta: [((2,2), Apple), ((4,3), Snake), ((4,4), SnakeHead)]
-- 

move :: BoardInfo -> GameState -> ([ Board.RenderMessage ] , GameState)
move board@(BoardInfo _row _col) state@(GameState (SnakeSeq sh sb) apple _ _) =
  let newHead = nextHead board state
      gameOver = newHead `elem` sb
      eatingApple = newHead == apple
  in
    case (gameOver, eatingApple) of
    (True, _) -> ([ Board.GameOver ], state)
    (_, True) -> case sb of
      S.Empty ->
        let newSnake = SnakeSeq (newHead) (S.singleton sh)
            (apple', newGen) = newApple board state
            changes = Board.RenderBoard [(newHead, Board.SnakeHead), (sh,Board.Snake),(apple', Board.Apple)]
            newState = state{snakeSeq = newSnake, randomGen = newGen, applePosition = apple'}
        in ([ changes, Board.UpdateScore 1 ], newState)
      xs ->
        let newSnake = SnakeSeq (newHead) (sh :<| xs)
            (apple', newGen) = newApple board state
            changes = Board.RenderBoard [(newHead, Board.SnakeHead), (sh,Board.Snake),(apple', Board.Apple)]
            newState = state{snakeSeq = newSnake, randomGen = newGen, applePosition = apple'}
        in ([ changes, Board.UpdateScore 1 ], newState)
    (_,_) -> case sb of
      S.Empty ->
        let newSnake = SnakeSeq newHead S.empty
            changes = Board.RenderBoard [(sh, Board.Empty), (newHead, Board.SnakeHead)]
            newState = state{snakeSeq = newSnake}
        in ([ changes ], newState)
      x :<| S.Empty ->
        let newSnake = SnakeSeq newHead (S.singleton sh)
            changes = Board.RenderBoard [(x, Board.Empty), (sh, Board.Snake), (newHead, Board.SnakeHead)]
            newState = state{snakeSeq = newSnake}
        in ([ changes ], newState)
      firstElement :<| (seq  :|> lastElement)   ->
        let newSnake = SnakeSeq newHead (sh :<| firstElement :<| seq)
            changes = Board.RenderBoard [(lastElement, Board.Empty), (sh, Board.Snake), (newHead, Board.SnakeHead)]
            newState = state{snakeSeq = newSnake}
        in ([ changes ], newState)
