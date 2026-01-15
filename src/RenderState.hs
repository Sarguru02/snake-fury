{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module RenderState where

import Data.Array ( (//), listArray, Array, elems )
import Data.Foldable ( foldl' )

type Point = (Int, Int)

data CellType =
    Empty
  | Snake
  | SnakeHead
  | Apple
  deriving (Show, Eq)

data BoardInfo = BoardInfo
  { height :: Int
  , width :: Int
  }
  deriving (Show, Eq)

type Board = Array Point CellType

type DeltaBoard = [(Point, CellType)]

data RenderMessage =
    RenderBoard DeltaBoard
  | GameOver
  deriving Show

data RenderState   = RenderState
  { board :: Board
  , gameOver :: Bool
  }
  deriving Show

emptyGrid :: BoardInfo -> Board
emptyGrid (BoardInfo rows cols) = listArray ((1,1),(rows,cols)) [Empty | _ <- [1..rows], _ <- [1..cols]]

buildInitialBoard
  :: BoardInfo -- ^ Board size
  -> Point     -- ^ initial point of the snake
  -> Point     -- ^ initial Point of the apple
  -> RenderState
buildInitialBoard bi snake apple =
  let grid = emptyGrid bi
  in RenderState (grid//[(snake, SnakeHead), (apple, Apple)]) False

updateRenderState :: RenderState -> RenderMessage -> RenderState
updateRenderState (RenderState stateboard _) (GameOver) = RenderState stateboard True
updateRenderState (RenderState stateboard stateGo) (RenderBoard delta) =
  let newStateBoard = foldl' (\acc (pt, celltype) -> acc//[(pt, celltype)]) stateboard delta
  in RenderState newStateBoard stateGo

ppCell :: CellType -> String
ppCell Empty     = ". "
ppCell Snake     = "o "
ppCell SnakeHead = "+ "
ppCell Apple     = "@ "

render :: BoardInfo -> RenderState -> String
render info@(BoardInfo rows cols) (RenderState stateboard over) =
  if over
    then snd $ boardToString $ emptyGrid info
    else snd $ boardToString stateboard
  where
    boardToString board = foldl' fn (0, "") board
    fn (idx, str) ctype = if (idx+1) `mod` cols == 0 
      then (idx+1, str <> ppCell ctype <> "\n")
      else (idx+1, str <> ppCell ctype)
