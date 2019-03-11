module ListSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.List

propInsert (k,v) t =
  label (summarize k v t) $
  toList (insert k v t)
    ===
  insert (k,v) (deleteKey k $ toList t)
  where summarize k v t =
          if all (>=k) (keys t) then "at start" else
          if all (<=k) (keys t) then "at the end" else
                                     "middle"

spec :: Spec
spec = do
  describe "basic" $ do
    it "we are required to return a spec with hspec" $
       1 `shouldBe` 1
