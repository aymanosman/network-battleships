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
import qualified Debug.Trace as D

-- t = D.traceShowId

(|>) :: a -> (a -> b) -> b
x |> f = f x

type Point = (Int, Int)

data Cell = Empty | Ship | Missed | Hit deriving (Eq)

instance Show Cell where
  show Empty = "."
  show Ship = "S"
  show Missed = "O"
  show Hit = "X"

newtype Board = Board {unBoard :: Array Point Cell}
  deriving (Eq)

showRow :: Board -> Int -> String
showRow (Board board) y =
  concat $
  do x <- [1 .. 8]
     let v = board ! (x,y)
     return (show v)

instance Show Board where
  show board =
    intercalate "\n" $
    do y <- [1 .. 8]
       return $ showRow board y

lb, ub :: Point
lb = (1,1)
ub = (8,8)

emptyBoard :: Board
emptyBoard =
  Board $ listArray (lb, ub) (repeat Empty)

placeShip :: [Point] -> Board -> Board
placeShip ps (Board b) =
  Board $ b // zip ps (repeat Ship)

data Placement
  = Down Int Point
  | Across Int Point

placeShip' :: Placement -> Board -> Maybe Board
placeShip' p b =
  case p of
    Down n qq ->
      Just $ placeShip (D.traceShowId (pp n qq)) b

    Across n q ->
      Just $ placeShip (pp n q) b
  where
    pp n (x, y) = zip (replicate n x) [y..]

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

safeLookup :: Array Point e -> Point -> Maybe e
safeLookup a (x,y) =
  if (x > 0 && x <= 8) && (y > 0 && y <= 8)
     then Just $ a ! (x,y)
     else Nothing

fire :: Point -> Board -> Board
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
     let mCoords :: Maybe Point = (unpack <$> message) >>= readMay
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

m :: IO ()
m =
  do let x = placeShip' (Down 4 (1,1)) emptyBoard
     print x
