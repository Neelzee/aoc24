module Main where

import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

main :: IO ()
main = do
  str <- readFile "input.txt"
  let rawStones = words str
  let stones = mapMaybe (\c -> readMaybe c :: Maybe Int) rawStones
  let newStones = mapN 74 stone stones
  print $ length newStones

mapN :: Int -> (a -> [a]) -> [a] -> [a]
mapN 0 f xs = concatMap f xs
mapN n f xs = mapN (n - 1) f (concatMap f xs)

stone :: Int -> [Int]
stone 0 = [1]
stone x
  | even len = [first, lst]
  | otherwise = [x * 2024]
 where
  digits = digs x
  len = length digits
  first = undigs $ take (len `div` 2) digits
  lst = undigs $ drop (len `div` 2) digits

digs :: Int -> [Int]
digs n
  | n <= 9 = [n]
  | otherwise = n `mod` 10 : digs (n `div` 10)

undigs :: [Int] -> Int
undigs [] = 0
undigs (x : xs) = x + (undigs xs * 10)
