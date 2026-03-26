{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module RenderState where

import Data.Array ( (//), listArray, Array )

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
  | UpdateScore Int
  | GameOver
  deriving Show

data RenderState   = RenderState
  { board :: Board
  , gameOver :: Bool
  , score :: Int
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
  in RenderState (grid//[(snake, SnakeHead), (apple, Apple)]) False 0

updateRenderState :: RenderState -> RenderMessage -> RenderState
updateRenderState (RenderState stateboard _ sc) (GameOver) = RenderState stateboard True sc
updateRenderState (RenderState stateboard stateGo sc) (UpdateScore x) = RenderState stateboard stateGo (sc+x)
updateRenderState (RenderState stateboard stateGo sc) (RenderBoard delta) =
  let newStateBoard = foldl' (\acc (pt, celltype) -> acc//[(pt, celltype)]) stateboard delta
  in RenderState newStateBoard stateGo sc

updateRenderMessages :: RenderState -> [RenderMessage] -> RenderState
updateRenderMessages state = foldl' (\ acc msg -> updateRenderState acc msg ) state

ppCell :: CellType -> String
ppCell Empty     = ". "
ppCell Snake     = "o "
ppCell SnakeHead = "+ "
ppCell Apple     = "@ "

render :: BoardInfo -> RenderState -> String
render info@(BoardInfo _rows cols) (RenderState stateboard over _) =
  if over
    then snd $ boardToString $ emptyGrid info
    else snd $ boardToString stateboard
  where
    boardToString bo = foldl' fn (0, "") bo
    fn (idx, str) ctype = if (idx+1) `mod` cols == 0 
      then (idx+1, str <> ppCell ctype <> "\n")
      else (idx+1, str <> ppCell ctype)
