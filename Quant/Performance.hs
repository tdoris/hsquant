{-# LANGUAGE TemplateHaskell #-}
module Quant.Performance
(
  Transaction(..)
, Book(..)
, position
, markToMarket
)
where

import           Control.Lens        hiding (elements)
import           Control.Monad.State
import           Data.List
import           Quant.Base.Types
import           Quant.Decimal
import           Test.QuickCheck

type SplitRatio = Decimal

data Transaction = Trade Side Price Qty
                   |Dividend Amount
                   |Split SplitRatio
                    deriving (Eq, Show)

data Book = Book
  { _cash       :: Amount
  , _shareCount :: Qty
  } deriving (Eq, Show)

makeLenses ''Book

update :: Transaction -> State Book ()

update (Trade Buy p q) = do
  cash -= toAmount p q
  shareCount += q

update (Trade Sell p q) = do
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

markToMarket :: [Transaction] -> Price -> Amount
markToMarket ts p = pnl
  where 
    finalBook = position ts
    (pnl, _) = runState (markToMarketPnl p) finalBook 
--TODO generate Dividends and Splits
instance Arbitrary Transaction where
  arbitrary = do
    side <- elements [Buy, Sell]
    price <- arbitrary :: Gen Decimal
    qty <- arbitrary :: Gen Decimal
    return (Trade side (MkPrice $ abs price) (MkQty $ abs qty))

instance Arbitrary Book where
  arbitrary = do
    cash' <- arbitrary :: Gen Decimal
    position' <-  arbitrary :: Gen Decimal
    return (Book (MkAmount cash') (MkQty position'))

{-

closingTransaction :: Book -> Price -> Transaction
closingTransaction (Book _ qty) price
  | qty > 0   = Trade Sell price qty
  | otherwise = Trade Buy price (abs qty)
-}

--- Tests
--

