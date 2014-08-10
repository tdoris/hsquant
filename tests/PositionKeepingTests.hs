module Main(main) where

import Test.QuickCheck
import Quant.Performance
import Quant.Base.Types

main :: IO ()
main = runTests

runTests :: IO ()
runTests = do
  let testArgs = stdArgs {maxSuccess = 1000}
  quickCheckWith testArgs propSimpleStart

propSimpleStart :: Transaction -> Bool
propSimpleStart t@(Trade Buy tprice tqty) =
  position [t] == Book (- (toAmount tprice tqty)) tqty

propSimpleStart t@(Trade Sell tprice tqty) =
  position [t] == Book (toAmount tprice tqty) (-tqty)

propSimpleStart _ = True

-- (qty, price)

{-

tupleTransactions :: [(Double, Integer)]
tupleTransactions = [(1.21, 24), (1.30, -2)]
toTransaction :: (Double, Integer) -> Transaction
toTransaction (price, qty)
  | qty < 0 = Trade Sell (MkPrice $ read (show price)) (MkQty $ read (show (abs qty)))
  | otherwise = Trade Buy (MkPrice $ read (show price)) (MkQty $ read (show (abs qty)))

-}
