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
import           Data.List.Split

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

instance Show Board where
  show (Board b) =
    intercalate "\n"
    . map (concatMap show)
    . transpose
    . chunksOf 8
    $ elems b

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

-- placeShip' :: Placement -> Board -> Maybe Board
placeShip' :: Placement -> Board -> Board
placeShip' p b =
  case p of
    Down n (x, y) ->
      flip placeShip b
      $ zip (replicate n x) [y..]

    Across n (x, y) ->
      flip placeShip b
      $ zip [x..] (replicate n y)

initB :: Board
initB =
  emptyBoard
   |> placeShip' (Down 1 (1,5))
   |> placeShip' (Across 4 (5,2))
   |> placeShip' (Across 2 (6,5))
   |> placeShip' (Down 3 (3,6))


safeLookup :: Array Point e -> Point -> Maybe e
safeLookup a (x,y) =
  if (x > 0 && x <= 8) && (y > 0 && y <= 8)
     then Just $ a ! (x,y)
     else Nothing

updateCell :: Cell -> Point -> Board -> Board
updateCell newCell ix (Board b) =
  Board $ b // [(ix, newCell)]

fire :: Point -> Board -> Maybe Board
fire ix (Board board) =
  safeLookup board ix
  |> fmap (\cell -> updateCell (f cell) ix (Board board))
  -- case safeLookup board ix of
  --   Nothing -> Board board
  --   Just currentVal -> Board $ board // [(ix,f currentVal)]
  where f Empty = Missed
        f Ship = Hit
        f Missed = Missed
        f Hit = Hit

handleCommand :: Socket -> Board -> IO Board
handleCommand socket board =
  do send socket $ pack $ show board
     send socket "\nGive us your point, hint (1,1): "
     message <- recv socket 1500
     let mCoords :: Maybe Point = (unpack <$> message) >>= readMay
     case mCoords of
       Nothing ->
         do send socket "Unrecognised command\n"
            handleCommand socket board
       Just coords ->
         case fire coords board of
           Nothing ->
             do send socket "You missed the board ¬_¬\n"
                handleCommand socket board
           Just newBoard ->
             handleCommand socket newBoard

main :: IO ()
main =
  forever $
  serve (Host "0.0.0.0") "8000" $
  \(connectionSocket,_) ->
    forever $ handleCommand connectionSocket initB
