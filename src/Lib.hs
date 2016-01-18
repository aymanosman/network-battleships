{-# LANGUAGE ScopedTypeVariables #-}
module Lib (main) where

import           Control.Monad
import           Data.Array
import           Data.List
import           Safe

data Cell = Empty | Ship | Missed | Hit

instance Show Cell where
  show Empty = "_"
  show Ship = "_"
  show Missed = "."
  show Hit = "X"

newtype Board =
  Board {unboard :: Array (Int,Int) Cell}

size :: ((Int, Int), (Int, Int))
size = ((1,1),(8,8))

showRow :: Board -> Int -> String
showRow board y =
  concat $
  do x <- [1 .. 8]
     let v = (unboard board) ! (x,y)
     return (show v)

instance Show Board where
  show board =
    intercalate "\n" $
    do y <- [1 .. 8]
       return $ showRow board y

emptyBoard :: Board
emptyBoard = Board $ array size contents
  where contents =
          do x <- [1 .. 8]
             y <- [1 .. 8]
             return ((x,y),Empty)

initialBoard :: Board
initialBoard =
  Board $
  b //
  [((1,5),Ship)
  ,((5,2),Ship)
  ,((6,2),Ship)
  ,((7,2),Ship)
  ,((8,2),Ship)
  ,((6,5),Ship)
  ,((7,5),Ship)
  ,((3,6),Ship)
  ,((3,7),Ship)
  ,((3,8),Ship)]
  where (Board b) = emptyBoard

fire :: (Int,Int) -> Board -> Board
fire ix (Board board) = Board $ board // [(ix,newVal)]
  where currentVal = board ! ix
        newVal = f currentVal
        f Empty = Missed
        f Ship = Hit
        f Missed = Missed
        f Hit = Hit


handleCommand :: Board -> IO Board
handleCommand board =
  do print board
     putStrLn "Give us your point: "
     mCoords :: Maybe (Int,Int) <- readMay <$> getLine
     case mCoords of
       Nothing ->
         do putStrLn "Unrecognised command"
            handleCommand board
       Just coords -> handleCommand $ fire coords board

main :: IO b
main = forever $ handleCommand initialBoard

m :: IO ()
m = print (fire (1,5) initialBoard)
