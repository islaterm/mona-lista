-- "Mona Lista" (c) by Ignacio Slater M.

-- "Mona Lista" is licensed under a
-- Creative Commons Attribution 4.0 International License.

-- You should have received a copy of the license along with this
-- work. If not, see <http://creativecommons.org/licenses/by/4.0/>.

-- v1.0.0-b.2
module Lib where

import           Prelude

type Matrix = [[Int]]
type Size = Int
type RowIdx = Int
type ColIdx = Int

source :: Matrix
source = [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12], [13, 14, 15, 16]]

target :: Matrix
target = [[14, 16, 15, 13], [6, 8, 7, 5], [2, 4, 3, 1], [10, 12, 11, 9]]

swapM :: Int -> Int -> [[a]] -> [[a]]
swapM x y | x == y    = id
          | otherwise = swapM' (min x y) (max x y)

swapM' :: Int -> Int -> [[a]] -> [[a]]
swapM' first second lst = beginning ++ [y] ++ middle ++ [x] ++ end
 where
  (beginning, (x : r)  ) = splitAt first lst
  (middle   , (y : end)) = splitAt (second - first - 1) r

swap :: Int -> Int -> [a] -> [a]
swap x y | x == y    = id
         | otherwise = swap' (min x y) (max x y)

swap' :: Int -> Int -> [a] -> [a]
swap' first second lst = beginning ++ [y] ++ middle ++ [x] ++ end
 where
  (beginning, (x : r)  ) = splitAt first lst
  (middle   , (y : end)) = splitAt (second - first - 1) r


swapRow :: RowIdx -> RowIdx -> Matrix -> Matrix
swapRow row1 row2 matrix = swapM row1 row2 matrix

swapColumn :: ColIdx -> ColIdx -> Matrix -> Matrix
swapColumn col1 col2 matrix = if null matrix
  then []
  else [(swap col1 col2 row)] ++ (swapColumn col1 col2 (tail matrix))
  where row = head matrix

swapMUntil :: Size -> (Matrix -> Bool) -> (Int, [Matrix]) -> (Int, [Matrix])
swapMUntil = undefined

answer :: Int
answer = fst $ swapMUntil 4 (== target) (0, [source])
