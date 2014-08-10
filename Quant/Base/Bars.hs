{-# LANGUAGE ScopedTypeVariables #-} 

module Quant.Base.Bars 
(
createBars
, MinutesPerBar
, Bar(..)
)
where

import Data.List
import Data.Maybe (mapMaybe)
import Quant.Base.Types
import Quant.Base.Exchange
import Quant.Base.Utils

data Bar = Bar
  { barTime             :: TimeOfDay
  , barOpenBid          :: OpenBid
  , barLowBid           :: LowBid
  , barHighBid          :: HighBid
  , barCloseBid         :: CloseBid
  , barOpenAsk          :: OpenAsk
  , barLowAsk           :: LowAsk
  , barHighAsk          :: HighAsk
  , barCloseAsk         :: CloseAsk
  , barVWAP             :: Price
  , barVolume           :: Qty
  , barTWABidQty        :: TWABidQty
  , barTWAAskQty        :: TWAAskQty
  , barTradedBidQty     :: Qty
  , barTradedAskQty     :: Qty
  , barTradedUnknownQty :: Qty
  , barTradeCount       :: Count
  , barQuoteCount       :: Count
  }
  deriving (Eq, Show, Ord)


type MinutesPerBar = Int 
data BarInput = BarInput [ExchangeQuote] [ExchangeTrade]

-- | Create bars given the quotes and trades and the specified minutes per bar,
--   quotes and trades supplied should typically be all quotes and trades for a full day
--
createBars :: [ExchangeQuote] -> [ExchangeTrade] -> MinutesPerBar -> [Bar]
createBars [] _ _ = []
createBars quotes trades barMinutes = bars
  where 
    lastTimeStamp = maximum (lastExchangeQuoteStamp ++ lastExchangeTradeStamp)
    lastExchangeQuoteStamp = map quoteTime (take 1 $ reverse quotes)
    lastExchangeTradeStamp = map tradeTime (take 1 $ reverse trades)
    timeStamps = takeWhilePlusOne (<= lastTimeStamp) $ map TimeOfDay [7*60*60*1000,(7*60*60*1000+fromIntegral barMinutes * 60000)..]
    squotes = synthExchangeQuotes quotes timeStamps
    qs = sort (quotes ++ squotes)
    qbs = bucket (map (\t q -> quoteTime q < t) timeStamps) qs
    tbs = bucket (map (\t r -> tradeTime r < t) timeStamps) trades
    barInputs = zipWith BarInput qbs tbs
    bars = mapMaybe createBar barInputs

-- create a synthetic quote for each timestamp
synthExchangeQuotes :: [ExchangeQuote] -> [TimeOfDay] -> [ExchangeQuote]
synthExchangeQuotes [] _ = []
synthExchangeQuotes _ [] = []
synthExchangeQuotes quotes@(q:_) (t:ts) = if quoteTime q > t then synthExchangeQuotes quotes ts else squote : synthExchangeQuotes (q1:qs') ts
  where
    (q1:qs') = dropWhileLessOne (\qr -> quoteTime qr < t) quotes
    squote = q1{ quoteTime = t } 
    
-- quotes and trades are assumed to be ordered by timestamp 
createBar :: BarInput -> Maybe Bar
createBar (BarInput [] _) = Nothing
createBar (BarInput quotes trades) = 
  Just (Bar start openBid lowBid highBid closeBid openAsk lowAsk highAsk closeAsk 
            vwapExchangeTrades volume 0 0 tradedBidQty tradedAskQty (volume - (tradedBidQty + tradedAskQty))
            (genericLength trades) (genericLength quotes))
  where 
    start = quoteTime $ head quotes
    --bids = map quoteBid quotes 
    --asks = map quoteAsk quotes 
    qtys = map tradeQty trades
    (openBid, highBid, lowBid, closeBid) = undefined -- ohlc bids
    (openAsk, highAsk, lowAsk, closeAsk) = undefined -- ohlc asks
    --prices = map tradePrice trades
    vwapExchangeTrades = undefined -- if pqs == [] then 0.0 else undefined --vwap pqs (read "0.01")
    --pqs = zip prices qtys
    volume = sum qtys
    tradesOnBid = getExchangeTradesOnBid quotes trades
    tradesOnAsk = getExchangeTradesOnAsk quotes trades
    tradedAskQty = sum $ map tradeQty tradesOnAsk
    tradedBidQty = sum $ map tradeQty tradesOnBid 
    
getExchangeTradesOnAsk :: [ExchangeQuote] -> [ExchangeTrade] -> [ExchangeTrade] 
getExchangeTradesOnAsk qs ts = tradesOnAsk' qs ts []

tradesOnAsk' :: [ExchangeQuote] -> [ExchangeTrade] -> [ExchangeTrade] -> [ExchangeTrade]
tradesOnAsk' [] _ result = result
tradesOnAsk' _ [] result = result
tradesOnAsk' quotes (t:ts) result = 
  case quotes' of 
    [] -> result
    (q:_) -> if quoteTime q <= tradeTime t && quoteAsk q <= tradePrice t then tradesOnAsk' quotes' ts (t:result) else tradesOnAsk' quotes' ts result 
  where  
    quotes' = dropUntilNext (\q -> quoteTime q > tradeTime t) quotes 

getExchangeTradesOnBid :: [ExchangeQuote] -> [ExchangeTrade] -> [ExchangeTrade] 
getExchangeTradesOnBid qs ts = tradesOnBid' qs ts []

tradesOnBid' :: [ExchangeQuote] -> [ExchangeTrade] -> [ExchangeTrade] -> [ExchangeTrade]
tradesOnBid' [] _ result = result
tradesOnBid' _ [] result = result
tradesOnBid' quotes (t:ts) result = 
  case quotes' of 
    [] -> result
    (q:_) -> if quoteTime q <= tradeTime t && quoteBid q >= tradePrice t then tradesOnBid' quotes' ts (t:result) else tradesOnBid' quotes' ts result 
  where  
    quotes' = dropUntilNext (\q -> quoteTime q > tradeTime t) quotes 

