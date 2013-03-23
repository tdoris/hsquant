{-# LANGUAGE ScopedTypeVariables #-} 

module Quant.Base.Bars 
(
createBars
, MinutesPerBar
)
where

import Data.List
import Data.Maybe (mapMaybe)
import Quant.Base.Types
import Quant.Base.Metrics
import Quant.Base.Utils


type MinutesPerBar = Int 
data BarInput = BarInput [Quote] [Trade]

-- | Create bars given the quotes and trades and the specified minutes per bar,
--   quotes and trades supplied should typically be all quotes and trades for a full day
--
createBars :: [Quote] -> [Trade] -> MinutesPerBar -> [Bar]
createBars [] _ _ = []
createBars quotes trades barMinutes = bars
  where 
    lastTimeStamp = maximum (lastQuoteStamp ++ lastTradeStamp)
    lastQuoteStamp = map quoteTime (take 1 $ reverse quotes)
    lastTradeStamp = map tradeTime (take 1 $ reverse trades)
    timeStamps = takeWhilePlusOne (<= lastTimeStamp) $ map TimeOfDay [7*60*60*1000,(7*60*60*1000+fromIntegral barMinutes * 60000)..]
    squotes = synthQuotes quotes timeStamps
    qs = sort (quotes ++ squotes)
    qbs = bucket (map (\t q -> quoteTime q < t) timeStamps) qs
    tbs = bucket (map (\t r -> tradeTime r < t) timeStamps) trades
    barInputs = zipWith BarInput qbs tbs
    bars = mapMaybe createBar barInputs

-- create a synthetic quote for each timestamp
synthQuotes :: [Quote] -> [TimeOfDay] -> [Quote]
synthQuotes [] _ = []
synthQuotes _ [] = []
synthQuotes quotes@(q:_) (t:ts) = if quoteTime q > t then synthQuotes quotes ts else squote : synthQuotes (q1:qs') ts
  where
    (q1:qs') = dropWhileLessOne (\qr -> quoteTime qr < t) quotes
    squote = q1{ quoteTime = t } 
    
-- quotes and trades are assumed to be ordered by timestamp 
createBar :: BarInput -> Maybe Bar
createBar (BarInput [] _) = Nothing
createBar (BarInput quotes trades) = 
  Just (Bar start openBid lowBid highBid closeBid openAsk lowAsk highAsk closeAsk 
            vwapTrades volume 0 0 tradedBidQty tradedAskQty (volume - (tradedBidQty + tradedAskQty))
            (genericLength trades) (genericLength quotes))
  where 
    start = quoteTime $ head quotes
    bids = map quoteBid quotes 
    asks = map quoteAsk quotes 
    qtys = map tradeQty trades
    (openBid, highBid, lowBid, closeBid) = ohlc bids
    (openAsk, highAsk, lowAsk, closeAsk) = ohlc asks
    prices = map tradePrice trades
    vwapTrades = if pqs == [] then 0.0 else vwap pqs (read "0.01")
    pqs = zip prices qtys
    volume = sum qtys
    tradesOnBid = getTradesOnBid quotes trades
    tradesOnAsk = getTradesOnAsk quotes trades
    tradedAskQty = sum $ map tradeQty tradesOnAsk
    tradedBidQty = sum $ map tradeQty tradesOnBid 
    
getTradesOnAsk :: [Quote] -> [Trade] -> [Trade] 
getTradesOnAsk qs ts = tradesOnAsk' qs ts []

tradesOnAsk' :: [Quote] -> [Trade] -> [Trade] -> [Trade]
tradesOnAsk' [] _ result = result
tradesOnAsk' _ [] result = result
tradesOnAsk' quotes (t:ts) result = 
  case quotes' of 
    [] -> result
    (q:_) -> if quoteTime q <= tradeTime t && quoteAsk q <= tradePrice t then tradesOnAsk' quotes' ts (t:result) else tradesOnAsk' quotes' ts result 
  where  
    quotes' = dropUntilNext (\q -> quoteTime q > tradeTime t) quotes 

getTradesOnBid :: [Quote] -> [Trade] -> [Trade] 
getTradesOnBid qs ts = tradesOnBid' qs ts []

tradesOnBid' :: [Quote] -> [Trade] -> [Trade] -> [Trade]
tradesOnBid' [] _ result = result
tradesOnBid' _ [] result = result
tradesOnBid' quotes (t:ts) result = 
  case quotes' of 
    [] -> result
    (q:_) -> if quoteTime q <= tradeTime t && quoteBid q >= tradePrice t then tradesOnBid' quotes' ts (t:result) else tradesOnBid' quotes' ts result 
  where  
    quotes' = dropUntilNext (\q -> quoteTime q > tradeTime t) quotes 

