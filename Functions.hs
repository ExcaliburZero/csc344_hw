module Functions where

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
