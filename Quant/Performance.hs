module Quant.Performance
(Quant.Performance.runTests)
where

import           Data.List
import           Quant.Base.Types
import           Quant.Decimal
import           Test.QuickCheck

type SplitRatio = Decimal
type Amount = Decimal
data Side = Buy | Sell deriving (Eq, Show)

data Transaction = Transaction {transSide::Side, transPrice::Price, transQty::Qty}
                   |Dividend Amount
                   |Split SplitRatio
                    deriving (Eq, Show)

instance Arbitrary Transaction where
  arbitrary = do
    side <- elements [Buy, Sell]
    price <- arbitrary :: Gen Decimal
    qty <- arbitrary :: Gen Decimal
    return (Transaction side (MkPrice $ abs price) (MkQty $ abs qty))

data State = State
  { sCash     :: Amount
  , sPosition :: Qty
  } deriving (Eq, Show)

instance Arbitrary State where
  arbitrary = do
    cash <- arbitrary :: Gen Decimal
    position <-  arbitrary :: Gen Decimal
    return (State cash (MkQty position))

position :: [Transaction] -> State
position = foldl' ptrans (State (0::Decimal) (MkQty 0))

ptrans :: State -> Transaction -> State
ptrans (State cash (MkQty shares)) (Dividend amt) = State (cash + amt*shares) (MkQty shares)
ptrans (State cash (MkQty shares)) (Split ratio) = State cash (MkQty $ shares*ratio)
ptrans (State cash shares) (Transaction Buy price qty) =
  State (cash - (calcAmount price qty)) (shares + qty)
ptrans (State cash shares) (Transaction Sell price qty) = State (cash + (calcAmount price  qty)) (shares -qty)

calcAmount :: Price -> Qty -> Amount
calcAmount (MkPrice p) (MkQty q) = p*q

closingTransaction :: State -> Price -> Transaction
closingTransaction (State _ qty) price
  | qty > 0   = Transaction Sell price qty
  | otherwise = Transaction Buy price (abs qty)

markToMarketPnl :: State -> Price -> Amount
markToMarketPnl (State cash qty) marketPrice = cash + (calcAmount marketPrice qty)

--- Tests
--
propSimpleStart :: Transaction -> Bool
propSimpleStart t@(Transaction Buy tprice tqty) =
  ptrans (State 0 0) t == State (- (calcAmount tprice tqty)) tqty

propSimpleStart t@(Transaction Sell tprice tqty) =
  ptrans (State 0 0) t == State (calcAmount tprice tqty) (-tqty)

propSimpleStart _ = True

-- (qty, price)
tupleTransactions = [(1.21, 24), (1.30, -2)]
toTransaction :: (Double, Integer) -> Transaction
toTransaction (price, qty) 
  | qty < 0 = Transaction Sell (MkPrice $ read (show price)) (MkQty $ read (show (abs qty)))
  | otherwise = Transaction Buy (MkPrice $read (show price)) (MkQty $ read (show (abs qty)))

runTests :: IO ()
runTests = do
  let testArgs = stdArgs {maxSuccess = 1000}
  --quickCheckWith testArgs propNoFreeMoney
  quickCheckWith testArgs propSimpleStart

