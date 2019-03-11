module Data.Coin where

newtype Coin = Coin Int
  deriving (Show, Eq)

maxCoinValue :: Int
maxCoinValue = 1000000

validCoin :: Coin -> Bool
validCoin (Coin n) =
  0 <= n && n <= maxCoinValue


-- v0
-- add :: Coin -> Coin -> Maybe Coin
-- add (Coin a) (Coin b) =
--   if a+b < maxCoinValue
--      then Just (Coin (a+b))
--      else Nothing

-- v1 fix off by one
add :: Coin -> Coin -> Maybe Coin
add (Coin a) (Coin b) =
  if a+b <= maxCoinValue
     then Just (Coin (a+b))
     else Nothing
