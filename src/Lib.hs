{-# LANGUAGE FlexibleInstances   #-}
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

newtype Board a =
  Board (Array (Int,Int) a)

instance Show v => Show (Board v) where
  show (Board board) =
    intercalate "\n" $
    do y <- range yRange
       return $ showRow y
    where showRow y =
            concat $
            do x <- range xRange
               return $ show (board ! (x,y))
          (xRange,yRange) = bounds board

emptyBoard :: ((Int,Int),(Int,Int)) -> Board Cell
emptyBoard size = Board (array size contents)
  where contents =
          do ix <- range size
             return (ix,Empty)

initialBoard :: Board Cell
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
  where (Board b) = emptyBoard ((1,1),(8,8))

safeLookup :: Ix i
           => i -> Array i e -> Maybe e
safeLookup ix a =
  if inRange (bounds a) ix
     then Just $ a ! ix
     else Nothing

safeReplace
  :: Ix i
  => (e -> e) -> i -> Array i e -> Array i e
safeReplace f ix a =
  case safeLookup ix a of
    Nothing -> a
    Just currentVal -> a // [(ix,f currentVal)]

dropBomb :: Cell -> Cell
dropBomb Empty = Missed
dropBomb Ship = Hit
dropBomb Missed = Missed
dropBomb Hit = Hit

fire :: (Int, Int) -> Board Cell -> Board Cell
fire ix (Board board) = Board $ safeReplace dropBomb ix board

handleCommand
  :: Board Cell -> Socket -> SockAddr -> IO ()
handleCommand board socket addr =
  do send socket . pack $ show board
     send socket "\nGive us your point: "
     message <- recv socket 1500
     let mCoords = (unpack <$> message) >>= readMay
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
     handleCommand newBoard socket addr

main :: IO ()
main =
  forever . serve (Host "0.0.0.0") "8000" $
  \(connectionSocket,addr) ->
    do printf "Connection from: %s\n" (show addr)
       handleCommand initialBoard connectionSocket addr
