-- http://hspec.github.io/quickcheck.html
-- http://blog.nikosbaxevanis.com/2015/01/30/quickcheck-setup-in-haskell/
module QuickCheckSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "icebreak" $ do
    prop "icebreaker" $ \x y ->
      add x y == add y x

add :: Int -> Int -> Int
add x y = x + y
