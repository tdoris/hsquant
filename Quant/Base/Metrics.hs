module Quant.Base.Metrics
(
 vwap
,ohlc
)
where

import Data.Ord
import Data.List
import Quant.Base.Types
import Quant.Decimal

-- | calculate the volume weighted average price
--

vwap :: [(Price, Qty)] -> Maybe Price
vwap [] = Nothing 
vwap ts = 
    case divide amount qty of
      (_, v):_ -> Just v
      _ -> Nothing
  where
    (MkAmount amount, MkQty qty) = sums ts
    sums :: [(Price, Qty)] -> (Amount, Qty)
    sums ts = (sum $ map (curry toAmount) ts, sum $ map snd ts)

-- | calculate open, high, low, and close
ohlc :: [Price] -> Maybe (Price, Price, Price, Price)
ohlc [] = Nothing 
ohlc ds = Just (head ds, maximum ds, minimum ds, last ds)

-- linspace :: Price -> TickSize -> Price -> [Price]
-- linspace pstart tick pend = takeWhile (pend>=) $ iterate (\p -> p + tick) pstart

--return the element that minimizes the function
-- sortByRange :: Ord a => (a->a) -> [a] -> [a]
-- sortByRange f ds = map fst $ sortBy (comparing snd) $ zip ds (map f ds)
