-- |
-- Student Name:        Christopher Wells
-- Assignment Number:   4
-- Due Date:            Dec. 8, 2016
module Functions where

import Data.List (splitAt)
import System.Random (randomRIO)

-- | Applies the given function three times to the given value.
--
-- >>> irepeat (+1) 0
-- 3
--
-- >>> irepeat (++ "!") "Hello, World"
-- "Hello, World!!!"
--
-- >>> irepeat (map (+1)) [1,2,3]
-- [4,5,6]
irepeat :: (a -> a) -> a -> a
irepeat f = f . f . f

-- | Replaces all instances of the given character in the given string with two
-- instances of the character.
--
-- >>> dup 'a' "the cat in the hat has a fat head"
-- "the caat in the haat haas aa faat heaad"
--
-- >>> dup '1' "1010"
-- "110110"
--
-- >>> dup '~' "~~Hello, World!!~~"
-- "~~~~Hello, World!!~~~~"
dup :: Char -> String -> String
dup _ [] = []
dup c (x:xs)
  | c == x    = c : c : dup c xs
  | otherwise = x :     dup c xs

-- | Replaces items i through j with a replacement list.
--
-- >>> replace [1,2,3] (4,5) [100,200,300,400,500,600,700]
-- [100,200,300,400,1,2,3,700]
--
-- >>> replace "big moneyed interests" (32,37) "a vote for me is a vote for the people"
-- "a vote for me is a vote for the big moneyed interests"
--
-- >>> replace [Just 1, Just 2] (2, 4) [Nothing, Nothing, Nothing, Nothing, Nothing]
-- [Nothing,Nothing,Just 1,Just 2]
--
-- >>> replace [1,2,3] (4,5) []
-- []
replace :: [a] -> (Int, Int) -> [a] -> [a]
replace _ _      []   = []
replace r (b, e) list = start ++ r ++ end
  where
    (_    , end) = splitAt (e + 1) list
    (start, _  ) = splitAt b       list

-- | Returns the prime factors of the given number.
--
-- >>> factors 2
-- [2]
--
-- >>> factors 27
-- [3,3,3]
--
-- >>> factors 89233
-- [17,29,181]
--
-- >>> factors 84234324234
-- [2,3,19,137,2269,2377]
factors :: Integer -> [Integer]
factors n = factors' n 2 []

factors' :: Integer -> Integer -> [Integer] -> [Integer]
factors' 1 _ fs = fs
factors' n d fs
  | n `mod` d == 0 = factors' (n `div` d) d (fs ++ [d])
  | otherwise      = factors' n (d + 1) fs

-- | The function used to generate a curve.
type CurveFunction = (Double -> Double)

-- | An (X, Y) point on a graph.
type Point = (Double, Double)

-- | A bound on a graph.
type Bound = (Double, Double)

-- | Runs a Monte Carlo simulation to determine the area under the curve of the
-- given graph in the given interval. Also takes in an upper y bound.
monte :: CurveFunction -> Bound -> Double -> IO Double
monte f ab yMax = monte' f ab yMax points
  where
    points = 100000

-- | Runs a Monte Carlo simulation for area under a curve, allowing for the
-- specification of the number of test points to use.
monte' :: CurveFunction -> Bound -> Double -> Integer -> IO Double
monte' f (a, b) yMax n = do
  let randomPt = randomPoint (a, b) (0, yMax)
  points <- mapM (const randomPt) [1..n]
  let under       = filter (underCurve f) points
  let totalPoints = realToFrac n
  let pointsUnder = realToFrac $ length under
  let area        = (b - a) * yMax
  return $ area * (pointsUnder / totalPoints)

-- | Returns a random (X, Y) point within the given X and Y bounds.
randomPoint :: Bound -> Bound -> IO Point
randomPoint (x1, x2) (y1, y2) = do
  randomX <- randomRIO (x1, x2) ::  IO Double
  randomY <- randomRIO (y1, y2) ::  IO Double
  return (randomX, randomY)

-- | Checks whether the given point is under the given curve.
--
-- >>> underCurve id (1, 0.5)
-- True
--
-- >>> underCurve id (1, 2)
-- False
underCurve :: CurveFunction -> Point -> Bool
underCurve f (x, y) = y < f x
