{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Quant.Base.Types 
(
 Bar(..)
, Quote(..)
, Trade(..)
, TWABidQty
, TWAAskQty
, RIC
, Root 
, TimeStamp 
, Price(..)
, Qty(..)
, TickSize
, Bid
, Ask 
, BidQty 
, AskQty
, Date(..) 
, Year 
, Month 
, Day 
, OpenBid 
, CloseBid 
, OpenAsk 
, CloseAsk 
, LowBid 
, HighBid 
, LowAsk 
, HighAsk 
, TimeOfDay(..) 
, parseDate
, parseTime
, getPrice
, getQty
, getTime
, getCount
)
where

import Control.Applicative
import qualified Data.Attoparsec.Char8 as P
import qualified Data.ByteString.Char8 as BS
import Quant.Decimal

newtype Price = MkPrice Decimal deriving (Show,Eq,Ord,Num)
newtype Qty = MkQty Decimal deriving (Show,Eq,Ord,Num)
newtype Count = MkCount Decimal deriving (Show,Eq,Ord,Num)
newtype TickSize = MkTickSize Decimal deriving (Show,Eq,Ord)

type TWABidQty = Qty
type TWAAskQty = Qty
    
type RIC=String
type Root = FilePath

-- | simplistic representation of time of day as the number of milliseconds since midnight,
--   no assumptions about timezones or conversions 
data TimeOfDay = TimeOfDay Integer deriving (Eq,Ord,Show)

type TimeStamp = String

type Bid = Price
type Ask = Price
type BidQty = Qty
type AskQty = Qty
data Date = Date 
  { dateYear:: Year
  , dateMonth:: Month
  , dateDay:: Day
  } deriving (Eq, Ord, Show)

type Year = String
type Month = String
type Day = String

type OpenBid = Price
type CloseBid = Price
type OpenAsk = Price
type CloseAsk = Price
type LowBid = Price
type HighBid = Price
type LowAsk = Price
type HighAsk = Price

data Quote = Quote
  { quoteTime :: TimeOfDay
  , quoteBid :: Bid
  , quoteAsk :: Ask
  , quoteBidQty :: BidQty
  , quoteAskQty :: AskQty
  }
  deriving (Eq,Show,Ord)

data Trade = Trade 
  { tradeTime :: TimeOfDay 
  , tradePrice :: Price 
  , tradeQty :: Qty
  }
  deriving (Eq, Show, Ord)

data Bar = Bar 
  { barTime :: TimeOfDay 
  , barOpenBid :: OpenBid 
  , barLowBid :: LowBid 
  , barHighBid :: HighBid
  , barCloseBid :: CloseBid
  , barOpenAsk :: OpenAsk 
  , barLowAsk :: LowAsk 
  , barHighAsk :: HighAsk 
  , barCloseAsk :: CloseAsk 
  , barVWAP :: Price
  , barVolume :: Qty
  , barTWABidQty :: TWABidQty
  , barTWAAskQty :: TWAAskQty
  , barTradedBidQty :: Qty 
  , barTradedAskQty :: Qty
  , barTradedUnknownQty :: Qty
  , barTradeCount :: Count
  , barQuoteCount :: Count
  }
  deriving (Eq, Show, Ord)


-- functions to parse basic types from bytestrings

getPrice :: BS.ByteString -> Price
getPrice b = MkPrice $ parseDecimal b 

getQty :: BS.ByteString -> Qty
getQty b = MkQty $ parseDecimal b

getCount :: BS.ByteString -> Count
getCount b = MkCount $ parseDecimal b

getTime :: BS.ByteString -> TimeOfDay
getTime b = parseTime b 

parseTime :: BS.ByteString -> TimeOfDay
parseTime s = 
  case P.parseOnly aparseTime s of
    Right t -> t
    Left  e -> error ("Failed to parse time from :"++ BS.unpack s ++ " error:"++ e)

colon, period :: P.Parser Char
colon = P.satisfy (== ':')
period = P.satisfy (== '.')

aparseTime :: P.Parser TimeOfDay
aparseTime = do 
  h <- P.decimal <* colon
  m <- P.decimal <* colon
  s <- P.decimal <* period
  ms <- P.decimal
  return $ TimeOfDay (h*3600000 + m*60000 + s*1000 +ms)

parseDate :: BS.ByteString -> Date
parseDate s = 
  case P.parseOnly aparseDate s of
    Right d -> d
    Left  e -> error ("Failed to parse date, (YYYYMMDD) from:" ++ BS.unpack s ++ " error:" ++ e)

aparseDate :: P.Parser Date
aparseDate = do
  y <- P.take 4 
  m <- P.take 2
  d <- P.take 2
  return $ Date (BS.unpack y) (BS.unpack m) (BS.unpack d)

-- a Decimal can be either a signed integer or a signed floating point, but we don't support scientific notation
parseDecimal :: BS.ByteString -> Decimal
parseDecimal s = 
  case P.parseOnly aparseDecimal s of
    Right d -> d
    Left e -> error ("Failed to parse decimal:"++BS.unpack s ++ " error:" ++ e)

aparseDecimal :: P.Parser Decimal
aparseDecimal = do 
  n <- P.takeWhile (/= '.') 
  P.skipWhile (== '.')
  f <- P.takeByteString
  let e = BS.length f 
  let m = parseInteger (BS.append n f)
  --default to 7 decimal places, this avoids expensive roundTos later
  let (e', m') = if e < 7 then (7, m * (10 ^ (7 - e))) else (e,m) 
  return  $ Decimal (fromIntegral e') m'

parseInteger :: BS.ByteString -> Integer
parseInteger s = 
  case P.parseOnly P.decimal s of
    Right i -> i
    Left e -> error ("Failed to parse int from:" ++ BS.unpack s ++ " error:" ++ e)

