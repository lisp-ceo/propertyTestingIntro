-- https://www.youtube.com/watch?v=NcJOiQlzlXQ

module CoinSpec (spec
                , testNormal
                , testOverflow
                , propNormal
                , propOverflow
                , propValid) where

import Test.Hspec
import Test.QuickCheck
import Data.Coin

-- v0
-- Arbitrary definition defines the generator need for properties
-- instance Arbitrary Coin where
--   arbitrary =
--     Coin <$> choose (0, maxCoinValue)

-- v1
-- use generator to state cases you wish to bias
-- generalized, grand, unified property

instance Arbitrary Coin where
  arbitrary = do
    NonNegative n <- arbitrary
    Coin <$>
      oneof [return n, -- any number
             return (maxCoinValue-n), -- close to upper end of range
             choose (0, maxCoinValue)] -- close to lower end of range
  
-- Unit tests
testNormal =
  Coin 2 `add` Coin 2
  ===
  Just (Coin 4)

testOverflow =
  Coin maxCoinValue `add` Coin 1
  ===
  Nothing

-- Properties
-- v0
-- propNormal (Coin a) (Coin b) =
--   a + b < maxCoinValue ==>
--     Coin a `add` Coin b
--     ===
--     Just (Coin (a+b))

-- v1 only generate valid (Normal) coins to prevent
-- generator throwing away invalid coins
-- custom data type
data Normal = Normal Coin Coin
  deriving Show

-- custom generator
instance Arbitrary Normal where
  arbitrary = do
    Coin a <- arbitrary
    b <- choose (0, maxCoinValue - a)
    return $ Normal (Coin a) (Coin b)
  
propNormal (Normal (Coin a) (Coin b)) =
  Coin a `add` Coin b
  ===
  Just (Coin (a+b))

-- v0
-- propOverflow (Coin a) (Coin b) =
--   a + b > maxCoinValue ==>
--   Coin a `add` Coin b
--   === Nothing

-- v1 only generate valid (Overflow) coins to prevent
-- generator throwing away coins that do not overflow

data Overflow = Overflow Coin Coin
  deriving Show

instance Arbitrary Overflow where
  arbitrary = do
    Coin a <- arbitrary
    b <- choose (maxCoinValue-a+1, maxCoinValue)
    return $ Overflow (Coin a) (Coin b)

-- custom validator
propOverflow (Coin a) (Coin b) =
  Coin a `add` Coin b
  === Nothing

propValidNormal (Normal c c') =
  validCoin c && validCoin c'

-- To run failing example @ 913693
-- quickCheck . withMaxSuccess maxCoinValue $ propValidOverflow
propValidOverflow (Overflow c c') =
  validCoin c && validCoin c'

-- quickCheck propValid
propValid (Coin c) = validCoin (Coin c)

-- one property to rule them all - provides 100% coverage
-- but is simpler than overflow/normal
-- custom data types and generators
-- use labels to test your intuition about covering all cases
-- v0
-- propAdd (Coin a) (Coin b) =
--   label (summarize (a+b)) $
--   Coin a `add` Coin b
--   ===
--   if validCoin c
--     then Just c
--     else Nothing
--   where c = Coin (a+b)
--         summarize n
--           | abs (n-maxCoinValue) < 3 = "boundary"
--           | n <= maxCoinValue = "normal"
--           | n > maxCoinValue = "overflow"

-- v1
-- using classify tells quickcheck about each label sepparately,
-- propAdd (Coin a) (Coin b) =
--   classify (abs (n-maxCoinValue) < 3) "boundary" $
--   classify (n <= maxCoinValue)        "normal" $
--   classify (n > maxCoinValue)         "overflow" $
--   Coin a `add` Coin b
--   ===
--   if validCoin (Coin n)
--     then Just (Coin n)
--     else Nothing
--   where n = a + b
  
-- v2
-- replacing classification with a coverage requirement
-- to cause failing test: `quickCheck . checkCoverage $ propAdd`
propAdd (Coin a) (Coin b) =
  cover 5  (abs (n-maxCoinValue) < 3) "boundary" $
  cover 40 (n <= maxCoinValue)        "normal"   $
  cover 40 (n > maxCoinValue)         "overflow" $
  Coin a `add` Coin b
  ===
  if validCoin (Coin n)
    then Just (Coin n)
    else Nothing
  where n = a + b
  
-- λ > labelledExamples propAdd
-- *** Found example of boundary
-- Coin 1000000
-- Coin 0
-- Just (Coin 1000000) == Just (Coin 1000000)

-- *** Found example of normal
-- Coin 0
-- Coin 115105
-- Just (Coin 115105) == Just (Coin 115105)

-- *** Found example of overflow
-- Coin 999999
-- Coin 1000000
-- Nothing == Nothing

-- +++ OK, passed 100 tests:
-- 52% normal
-- 45% overflow
--  3% boundary
-- it :: ()

spec :: Spec
spec = do
  describe "basic" $ do
    it "we are required to return a spec with hspec" $
       1 `shouldBe` 1
