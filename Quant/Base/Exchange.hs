module Quant.Base.Exchange
(
  TickSize
, Bid
, Ask
, BidQty
, AskQty
, OpenBid
, CloseBid
, OpenAsk
, CloseAsk
, LowBid
, HighBid
, LowAsk
, HighAsk
, ExchangeQuote(..)
, ExchangeTrade(..)
, TWABidQty
, TWAAskQty
)

where

import           Quant.Base.Types
import           Quant.Base.DateTime
import           Quant.Decimal

newtype TickSize = MkTickSize Decimal deriving (Show,Eq,Ord)
data TickLadder = MkTickLadder {
    _minTick  :: Decimal
    , _ladder :: [(Price, TickSize)]
   } deriving (Show, Eq, Ord)

type TWABidQty = Qty
type TWAAskQty = Qty

type OpenBid = Price
type CloseBid = Price
type OpenAsk = Price
type CloseAsk = Price
type LowBid = Price
type HighBid = Price
type LowAsk = Price
type HighAsk = Price

type Bid = Price
type Ask = Price
type BidQty = Qty
type AskQty = Qty

data ExchangeQuote = ExchangeQuote
  { quoteTime   :: TimeOfDay
  , quoteBid    :: Bid
  , quoteAsk    :: Ask
  , quoteBidQty :: BidQty
  , quoteAskQty :: AskQty
  }
  deriving (Eq,Show,Ord)

data ExchangeTrade = ExchangeTrade
  { tradeTime  :: TimeOfDay
  , tradePrice :: Price
  , tradeQty   :: Qty
  }
  deriving (Eq, Show, Ord)

