-- "Mona Lista" (c) by Ignacio Slater M.

-- "Mona Lista" is licensed under a
-- Creative Commons Attribution 4.0 International License.

-- You should have received a copy of the license along with this
-- work. If not, see <http://creativecommons.org/licenses/by/4.0/>.

-- v1.0.0-b.1
module Lib where

type Matrix = [[Int]]
type Size = Int

source :: Matrix
source = [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12], [13, 14, 15, 16]]

target :: Matrix
target = [[14, 16, 15, 13], [6, 8, 7, 5], [2, 4, 3, 1], [10, 12, 11, 9]]

swapMUntil :: Size -> (Matrix -> Bool) -> (Int, [Matrix]) -> (Int, [Matrix])
swapMUntil = undefined

answer :: Int
answer = fst $ swapMUntil 4 (== target) (0, [source])
