{-# Language RecordWildCards #-}
module GameState where

import RenderState (BoardInfo (..), Point, DeltaBoard)
import qualified RenderState as Board
import Data.Sequence (Seq(..))
import qualified Data.Sequence as S
import System.Random (uniformR, RandomGen(split), StdGen, Random (randomR))
import Data.Maybe (isJust)

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
nextHead BoardInfo{height=h, width=w} GameState{movement=direction, snakeSeq=snake, ..} =
  -- I probably shouldn't wrap. I should make the game over instead ?
  -- But for now let it be. The snake comes through the other side.
  let wrap1 n x = ((x-1) `mod` n) + 1 -- wraps from [1..n]
      wrap (x,y) = (wrap1 h x, wrap1 w y)
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

move :: BoardInfo -> GameState -> (Board.RenderMessage , GameState)
move = undefined

{- This is a test for move. It should return

RenderBoard [((1,4),SnakeHead),((1,1),Snake),((1,3),Empty)]
RenderBoard [((2,1),SnakeHead),((1,1),Snake),((3,1),Apple)] ** your Apple might be different from mine
RenderBoard [((4,1),SnakeHead),((1,1),Snake),((1,3),Empty)]

-}

-- >>> snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
-- >>> apple_pos = (2,1) 
-- >>> board_info = BoardInfo 4 4
-- >>> game_state1 = GameState snake_seq apple_pos West (System.Random.mkStdGen 1)
-- >>> game_state2 = GameState snake_seq apple_pos South (System.Random.mkStdGen 1)
-- >>> game_state3 = GameState snake_seq apple_pos North (System.Random.mkStdGen 1)
-- >>> fst $ move board_info game_state1
-- >>> fst $ move board_info game_state2
-- >>> fst $ move board_info game_state3
