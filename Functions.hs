module Functions where

import Data.List (splitAt)

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
