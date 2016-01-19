{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib
       -- (main)
       where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Array
import           Data.ByteString.Char8  (pack, unpack)
import           Data.List
import           Network.Simple.TCP
import           Safe

(|>) :: a -> (a -> b) -> b
x |> f = f x
infixl 2 |>

-- ((x |> f) |> g) |> h i

data Cell = Empty | Ship | Missed | Hit deriving (Eq)

instance Show Cell where
  show Empty = "."
  show Ship = "S"
  show Missed = "O"
  show Hit = "X"

newtype Board = Board {unboard :: Array (Int,Int) Cell}
  deriving (Eq)

showRow :: Board -> Int -> String
showRow board y =
  concat $
  do x <- [1 .. 8]
     let v = unboard board ! (x,y)
     return (show v)

instance Show Board where
  show board =
    intercalate "\n" $
    do y <- [1 .. 8]
       return $ showRow board y

lb, ub :: (Int, Int)
lb = (1,1)
ub = (8,8)

emptyBoard :: Board
emptyBoard = Board $ listArray (lb, ub) (repeat Empty)

placeShip :: [(Int, Int)] -> Board -> Board
placeShip ps (Board b) =
  Board $
  b //
  zip ps (repeat Ship)

initB :: Board
initB =
  emptyBoard
  |> placeShip [(1, 5)]
  |> placeShip [(5,2) ,(6,2) ,(7,2) ,(8,2)]
  |> placeShip [(6,5), (7,5)]
  |> placeShip [(3,6), (3,7), (3,8)]

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

safeLookup :: Array (Int,Int) e -> (Int,Int) -> Maybe e
safeLookup a (x,y) =
  if (x > 0 && x <= 8) && (y > 0 && y <= 8)
     then Just $ a ! (x,y)
     else Nothing

fire :: (Int,Int) -> Board -> Board
fire ix (Board board) =
  case safeLookup board ix of
    Nothing -> Board board
    Just currentVal -> Board $ board // [(ix,f currentVal)]
  where f Empty = Missed
        f Ship = Hit
        f Missed = Missed
        f Hit = Hit

handleCommand :: MonadIO m => Socket -> Board -> m Board
handleCommand socket board =
  do send socket $ pack $ show board
     send socket "\nGive us your point: "
     message <- recv socket 1500
     let mCoords :: Maybe (Int,Int) = (unpack <$> message) >>= readMay
     case mCoords of
       Nothing ->
         do send socket "Unrecognised command"
            handleCommand socket board
       Just coords -> handleCommand socket $ fire coords board

main :: IO ()
main =
  forever $
  serve (Host "0.0.0.0") "8000" $
  \(connectionSocket,_) ->
    forever $ handleCommand connectionSocket initialBoard
