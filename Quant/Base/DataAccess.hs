{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Quant.Base.DataAccess
(
  Root
, RIC
, loadTrades
, loadQuotes
, loadBars
, escapeRIC
, tradesFile
, quotesFile
)
where

import qualified Data.ByteString.Char8 as BS
import           Data.Char
import           Data.List
import           Quant.Base.Bars
import           Quant.Base.CSV
import           Quant.Base.Exchange
import           Quant.Base.Types
import           Quant.Base.DateTime
import           System.FilePath

type Root = FilePath
type RIC = String

loadBars :: Root -> Date -> RIC-> IO [Bar]
loadBars root date ric' = do
  let ric = escapeRIC ric'
  let bf = barsFile root date ric
  barsFileContent <- loadCSVBZ bf
  let bars = parseBarsCSV barsFileContent
  return bars

-- | load quotes for a day from csv format file, given the root of the filesystem db
--
-- > loadQuotes "/data/trth" (Date "2012" "02" "14") "VOD.L"

loadQuotes' :: FilePath -> IO [ExchangeQuote]
loadQuotes' qf = do
  quotesCSV <- loadCSVBZ qf
  let quotes = parseQuotesCSV quotesCSV
  return quotes

loadQuotes :: Root -> Date -> RIC -> IO [ExchangeQuote]
loadQuotes root date ric' = do
  let ric = escapeRIC ric'
  let  qf = quotesFile root date ric
  (loadQuotes' qf)

-- | load trades for a day from csv format file, Root is the root of the filesystem db
--
-- > loadTrades "/data/trth" (Date "2012" "02" "14") "VOD.L"

loadTrades' ::FilePath -> IO [ExchangeTrade]
loadTrades' tf = do
  tradesCSV <- loadCSVBZ tf
  let trades = parseTradesCSV tradesCSV
  -- let onMarketTrades = filter isOnMarket trades
  return trades

loadTrades :: Root -> Date -> RIC -> IO [ExchangeTrade]
loadTrades root date ric' = do
  let ric = escapeRIC ric'
  let tf = tradesFile root date ric
  loadTrades' tf


genFName :: String->Root -> Date -> RIC -> FilePath
genFName stem root (Date year month day) ric  = root </> year </> month </> day </> stem </> ric <.> "csv" <.> "bz2"

tradesFile :: Root -> Date -> RIC -> FilePath
tradesFile = genFName "trades"

quotesFile :: Root -> Date -> RIC -> FilePath
quotesFile = genFName "quotes"

barsFile :: Root -> Date -> RIC -> FilePath
barsFile root (Date year month day) ric = root </> year </> month </> day </> "bars" </> ric <.> "csv" <.> "bz2"

parseBarsCSV :: CSVTable -> [Bar]
parseBarsCSV table = bars
  where
    cols = getColumns table ["time","volume"
      ,"bid_start","start_ask","volume_up","volume_dn","volume_neither","trades","quotes","vwap","high","low"]
    rows = transpose cols
    bars = map (\[time,volume,start_bid,start_ask,volume_up,volume_dn,volume_neither,tradeCount,quoteCount,vwap,_high,_low]
                   -> Bar (parseTime time) (getPrice start_bid) 0 0 0 (getPrice start_ask) 0 0 0 (getPrice vwap) (getQty volume)
                          0 0 (getQty volume_dn) (getQty volume_up) (getQty volume_neither) (getCount tradeCount) (getCount quoteCount)
               )
               rows

parseTradesCSV :: CSVTable -> [ExchangeTrade]
parseTradesCSV table = trades
  where
    cols = getColumns table ["Time[L]", "Price", "Volume", "Qualifiers"]
    rows' = transpose cols
    rows  = filter (\[_,_,_,q] -> isOnMarket q) rows'
    trades = map (\[t,p,q,_] -> ExchangeTrade (getTime t) (getPrice p) (getQty q)) rows

-- TODO fixme
isOnMarket :: BS.ByteString -> Bool
isOnMarket _ = True

data QuoteRow = QuoteRow
  { _quoteRowTime  :: BS.ByteString
  , quoteRowBid    :: BS.ByteString
  , quoteRowAsk    :: BS.ByteString
  , quoteRowBidQty :: BS.ByteString
  , quoteRowAskQty :: BS.ByteString
  } deriving (Eq, Show,Ord)

toQuote :: QuoteRow -> ExchangeQuote
toQuote (QuoteRow t bid ask bqty aqty) = ExchangeQuote (getTime t) (getPrice bid) (getPrice ask) (getQty bqty) (getQty aqty)

parseQuotesCSV :: CSVTable -> [ExchangeQuote]
parseQuotesCSV table = map toQuote quotes
  where
    cols' = getColumns table ["Time[L]", "Bid Price", "Ask Price", "Bid Size", "Ask Size"]
    rows = transpose cols'
    [time0, bidPrice0, askPrice0, bidQty0, askQty0] = head rows
    startQoute = QuoteRow time0 bidPrice0 askPrice0 bidQty0 askQty0
    quotes = dropWhile (not . isQuoteValid) $ reverse $ foldl' quoteUpdateWrap [startQoute] (tail rows)

isQuoteValid :: QuoteRow -> Bool
isQuoteValid (QuoteRow _ bid ask bqty aqty) = bid /= "" && ask /= "" && bqty /= "" && aqty /= ""

quoteUpdateWrap :: [QuoteRow] -> [BS.ByteString] -> [QuoteRow]
quoteUpdateWrap qs@(lastquote:_) row =
  case updateQuote lastquote row of
    Just newQuote -> newQuote : qs
    Nothing -> qs
quoteUpdateWrap qs _ = qs

updateQuote :: QuoteRow -> [BS.ByteString] -> Maybe QuoteRow
updateQuote lastQuote [time, bid, ask, bqty, aqty]=
  if doUpdate then Just newQuote else Nothing
  where
    doUpdate = bid /= "" || ask /= "" || bqty /= "" || aqty /= ""
    newQuote = QuoteRow time newBid newAsk newBidQty newAskQty
    newBid = if bid /= "" then bid else quoteRowBid lastQuote
    newAsk = if ask /= "" then ask else quoteRowAsk lastQuote
    newBidQty = if bqty /= "" then bqty else quoteRowBidQty lastQuote
    newAskQty = if aqty /= "" then aqty else quoteRowAskQty lastQuote

updateQuote _ _ = Nothing

escapeRIC :: RIC -> String
escapeRIC = concatMap esc
  where
    s = "0123456789abcdef"
    esc c = if isAlphaNum c then [c] else [ '_', s !! div (fromEnum c) 16, s !! mod (fromEnum c) 16 ]

