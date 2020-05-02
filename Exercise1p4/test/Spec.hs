{-# LANGUAGE GADTs #-}
import Lib
import Test.QuickCheck
import Control.Monad
class Map m where
    (<@>) :: (a -> b) -> m a -> m b
-- Carries a Proxy with a bunch of dictionaries

data Proxy a = Proxy

data ProxyA where
  ProxyA :: (Eq a, Show a, Arbitrary a, Function a, CoArbitrary a, Arbitrary a) => Proxy a -> ProxyA

-- | The functor identity law.
prop_exerciseOneLeft :: (Eq (a), Show (a), Eq (b), Show (b), Eq (c), Show (c)
                       , Arbitrary (a), Arbitrary (b), Arbitrary (c)
                       , Function (a), Function (b), Function (c))
                    => Proxy (a)
                    -> Proxy (b)
                    -> Proxy (c)
                    -> Fun (b, c) a
                    -> b
                    -> c
                    -> Bool
prop_exerciseOneLeft _ _ _ (Fn f1) x y = (((from_e1.to_e1) f1) (x,y)) == (f1 (x,y))

prop_exerciseOneRight :: (Eq (a), Show (a), Eq (b), Show (b), Eq (c), Show (c)
                       , Arbitrary (a), Arbitrary (b), Arbitrary (c)
                       , Function (a), Function (b), Function (c))
                    => Proxy (a)
                    -> Proxy (b)
                    -> Proxy (c)
                    -> Fun (b, c) a
                    -> b
                    -> c
                    -> Bool
prop_exerciseOneRight _ _ _ (Fn2 f1) x y = (((to_e1.from_e1) f1) x y) == (f1 x y)


-- pretty much all of the GADT stuff was lifted from https://stackoverflow.com/questions/37852439/is-it-possible-to-map-quickcheck-over-a-list-of-types
types :: [ ProxyA ]
types =
  [ ProxyA (Proxy :: Proxy (Int))
  , ProxyA (Proxy :: Proxy (Char))
  , ProxyA (Proxy :: Proxy (String))
  , ProxyA (Proxy :: Proxy (Integer))
  ]

main :: IO ()
main = do
  forM_ types3 $ \ ((ProxyA p1), (ProxyA p2), (ProxyA p3)) -> quickCheck $ do
    prop_exerciseOneLeft p1 p2 p3
    prop_exerciseOneRight p1 p2 p3
  where
    types3 = [(a,b,c) | a <- types, b <- types, c <- types]
