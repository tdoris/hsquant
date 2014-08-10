module Quant.Base.Metrics
(
-- vwap
--,ohlc
)
where

import Data.Ord
import Data.List
import Quant.Base.Types

-- This needs to be rewritten with a price type that is ticksize aware, 
-- now that we've made price, qty and ticksize distinct types
--
-- | calculate the weighted average price
-- This function assumes an exogenous tick size/resolution
-- the calculation is formulated as solving for the price 
-- Min err = abs (vwap * totalQty - sum $ zipWith (*) prices qtys)
-- where vwap is an element of linspace pmin tick pmax 
--
{-
vwap :: [(Price, Qty)] -> TickSize -> Price
vwap [] _ = error "vwap passed empty list"
vwap pqs tick = head ps
  where
    ps = sortByRange (err totalQty totalAmount) (linspace (minimum prices) tick (maximum prices))
    err totQty totAmt vwap' = abs $ vwap' * totQty - totAmt
    totalAmount = sum $ zipWith (*) prices qtys
    prices = map fst pqs
    qtys = map snd pqs
    totalQty = sum qtys

-- | calculate open, high, low, and close
ohlc :: Ord a =>  [a] -> (a, a, a, a)
ohlc [] = error "empty list passed to ohlc"
ohlc ds = (head ds, maximum ds, minimum ds, last ds)

linspace :: Price -> TickSize -> Price -> [Price]
linspace pstart tick pend = takeWhile (pend>=) $ iterate (\p -> p + tick) pstart

--return the element that minimizes the function
sortByRange :: Ord a => (a->a) -> [a] -> [a]
sortByRange f ds = map fst $ sortBy (comparing snd) $ zip ds (map f ds)
-}
