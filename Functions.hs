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
