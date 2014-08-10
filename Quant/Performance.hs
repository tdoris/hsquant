{-# LANGUAGE TemplateHaskell #-}
module Quant.Performance
(Quant.Performance.runTests)
where

import           Control.Lens        hiding (elements)
import           Control.Monad.State
import           Data.List
import           Quant.Base.Types
import           Quant.Decimal
import           Test.QuickCheck

type SplitRatio = Decimal
data Side = Buy | Sell deriving (Eq, Show)

data Transaction = Transaction {transSide::Side, transPrice::Price, transQty::Qty}
                   |Dividend Amount
                   |Split SplitRatio
                    deriving (Eq, Show)

data Book = Book
  { _cash       :: Amount
  , _shareCount :: Qty
  } deriving (Eq, Show)

makeLenses ''Book

update :: Transaction -> State Book ()

update (Transaction Buy p q) = do
  cash -= toAmount p q
  shareCount += q

update (Transaction Sell p q) = do
  cash += toAmount p q
  shareCount -= q

update (Dividend amt) = do
  -- check if there's a way to do this in one line
  count <- use shareCount
  cash += toAmount amt count 

update (Split ratio) = do
  shareCount *= MkQty ratio

processTransaction :: Book -> Transaction -> Book
processTransaction b t = b'
  where
    (_, b') = runState (update t) b

position :: [Transaction] -> Book
position = foldl' processTransaction (Book 0 0)

markToMarketPnl :: Price -> State Book Amount
markToMarketPnl p = do
  s <- gets _shareCount
  c <- gets _cash
  return (c + toAmount p s)

--TODO generate Dividends and Splits
instance Arbitrary Transaction where
  arbitrary = do
    side <- elements [Buy, Sell]
    price <- arbitrary :: Gen Decimal
    qty <- arbitrary :: Gen Decimal
    return (Transaction side (MkPrice $ abs price) (MkQty $ abs qty))

instance Arbitrary Book where
  arbitrary = do
    cash <- arbitrary :: Gen Decimal
    position <-  arbitrary :: Gen Decimal
    return (Book (MkAmount cash) (MkQty position))

{-

closingTransaction :: Book -> Price -> Transaction
closingTransaction (Book _ qty) price
  | qty > 0   = Transaction Sell price qty
  | otherwise = Transaction Buy price (abs qty)
-}

--- Tests
--
propSimpleStart :: Transaction -> Bool
propSimpleStart t@(Transaction Buy tprice tqty) =
  processTransaction (Book 0 0) t == Book (- (toAmount tprice tqty)) tqty

propSimpleStart t@(Transaction Sell tprice tqty) =
  processTransaction (Book 0 0) t == Book (toAmount tprice tqty) (-tqty)

propSimpleStart _ = True

-- (qty, price)
tupleTransactions = [(1.21, 24), (1.30, -2)]
toTransaction :: (Double, Integer) -> Transaction
toTransaction (price, qty)
  | qty < 0 = Transaction Sell (MkPrice $ read (show price)) (MkQty $ read (show (abs qty)))
  | otherwise = Transaction Buy (MkPrice $ read (show price)) (MkQty $ read (show (abs qty)))

runTests :: IO ()
runTests = do
  let testArgs = stdArgs {maxSuccess = 1000}
  quickCheckWith testArgs propSimpleStart

