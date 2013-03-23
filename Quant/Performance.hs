module Quant.Performance 
()
where

import Data.List
import Quant.Base.Types
import Quant.Decimal

type Position = Decimal
type SplitRatio = Decimal
type Amount = Decimal
data SDate = SDate String deriving (Eq, Ord, Show)

psgle1= [23.3537, 41.5324, 36.855, 49.4749, 50.8871, 46.3, 46.93, 37.13, 45.03, 44.425, 41.4, 42.15, 36.33, 25.125, 21.95, 18.96, 23.155, 17.675]
psaca = [ 7.213, 10.67, 8.876, 12.93, 14.8, 11.98, 13.425, 9.731, 10.755, 11.675, 9.88, 11.08, 8.746, 6.755, 6.511, 4.917, 5.6, 4.965]

gleqty = [ 2141, -2141, -1357, 1357, 983, -983, 1065, -1065, -1110, 1110, -1208, 1208, -1376, 1376, 2278, -2278, -2159, 2159]
acaqty = [ -6932, 6932, 5635, -5635, -3380, 3380, -3723, 3723, 4647, -4647, 5062, -5062, 5716, -5716, -7680, 7680, 8927, -8927]

dates = map (SDate . show) [20090120, 20090601, 20090630, 20090904, 20091006, 20091218, 20100406, 20100518, 20100728, 20100920, 20101220, 20110512, 20110711, 20110808, 20110825, 20110916, 20111012, 20111101 ]

gleTransactions = map (\(p,q,d) -> Transaction (if q>0 then Buy else Sell) d p (abs q)) $ zip3 psgle1 gleqty dates
acaTransactions = map (\(p,q,d) -> Transaction (if q>0 then Buy else Sell) d p (abs q)) $ zip3 psaca acaqty dates

-- After each transaction we have state: (pnl, average_entry, signed_position)
data State = State 
  { sPnl :: Amount
  , sAverageEntryPrice :: Price
  , sPosition :: Position
  } deriving (Eq, Show)

data Side = Buy | Sell deriving (Eq, Show)

data Transaction = Transaction {transSide::Side, transDate::SDate, transPrice::Price, transQty::Qty}
                   |Dividend Amount
                   |Split SplitRatio
                    deriving (Eq, Show)

position :: [Transaction] -> State
position = foldl' ptrans (State 0 0.0 0.0)

ptrans :: State -> Transaction -> State
ptrans (State pnl avgPrice shares) t = State pnl' avgPrice' shares'
  where pnl' = pnl + if adding then 0 else transPnL
        avgPrice' = if adding then (abspos*avgPrice + notional t)/ (transQty t + abspos)
                              else case compare (transQty t) abspos of
                                        GT -> transPrice t
                                        LT -> avgPrice
                                        EQ -> 0.0
        shares' = shares + if transSide t == Sell then (- transQty t) else transQty t
        adding = transSide t == Sell && shares <= 0  ||transSide t == Buy && shares >= 0
        reduceQty = if adding then 0 else min (transQty t) abspos
        transPnL = reduceQty * if transSide t == Sell then transPrice t - avgPrice else avgPrice - transPrice t
        abspos = abs shares

ptrans (State pnl avgPrice shares) (Dividend amt) = State (pnl + amt*shares) avgPrice shares
ptrans (State pnl avgPrice shares) (Split ratio) = State pnl (avgPrice/ratio) (shares*ratio)

closingTransaction :: [Transaction] -> Price -> SDate -> [Transaction]
closingTransaction ts p date = if pos == 0 then [] else [Transaction { transSide = if pos > 0 then Sell else Buy, transDate=date, transPrice=p, transQty = abs pos }]
  where (State _ _ pos) = position ts

calcPnl :: [Transaction] -> Price -> SDate -> Amount
calcPnl ts currentPrice currentDate = profit
  where (State profit _ _) = position ts'
        ts' = ts ++ closingTransaction ts currentPrice currentDate

notional :: Transaction -> Amount
notional t = transPrice t * transQty t

