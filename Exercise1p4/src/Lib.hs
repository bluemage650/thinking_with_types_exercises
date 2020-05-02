module Lib
    ( from_e1,
      to_e1
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- use curry howard to prove that (a^b)^c = a^(b^c)

from_e1 :: (b -> c -> a) -> (b,c) -> a
from_e1 f (t1,t2) = f t1 t2

from_e1' :: (b -> c -> a) -> (b,c) -> a
from_e1' = uncurry

to_e1 :: ((b,c) -> a) -> b -> c -> a
to_e1 f b c = f (b,c)

to_e1' :: ((b,c) -> a) -> b -> c -> a
to_e1' = curry
