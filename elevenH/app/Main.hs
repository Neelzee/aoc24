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
  | even $ length $ show x = mapMaybe (\c -> readMaybe c :: Maybe Int) [first, lst]
  | otherwise = [x * 2024]
 where
  first = take (length (show x) `div` 2) (show x)
  lst = drop (length (show x) `div` 2) (show x)
