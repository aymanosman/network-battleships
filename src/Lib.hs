{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib (main) where

import           Control.Monad
import           Data.Array
import           Data.ByteString.Char8 (pack, unpack)
import           Data.List
import           Network.Simple.TCP
import           Safe
import           Text.Printf


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
     let v = unboard board ! (x,y)
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

handleCommand :: Socket -> SockAddr -> Board -> IO ()
handleCommand socket addr board =
  do send socket $ pack $ show board
     send socket "\nGive us your point: "
     message <- recv socket 1500
     let mCoords :: Maybe (Int,Int) = (unpack <$> message) >>= readMay
     newBoard <-
       case mCoords of
         Nothing ->
           do send socket "Unrecognised command"
              return board
         Just coords -> return $ fire coords board
     printf "Got move from %s: %s\n%s\n=======\n"
            (show addr)
            (show mCoords)
            (show newBoard)
     handleCommand socket addr newBoard

main :: IO ()
main =
  forever $
  serve (Host "0.0.0.0") "8000" $
  \(connectionSocket,addr) ->
    do printf "Connection from: %s\n" (show addr)
       handleCommand connectionSocket addr initialBoard
