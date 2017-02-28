module Quant.Base.Exchange
(
  TickSize
, upTick
, downTick
, mkTickLadder
, isValidPrice
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
import Data.List (sort)

-- NB: parse fails if spaces after comma in tuple of Decimals 
-- e.g. read "(10, 10)" :: (Decimal, Decimal) -> parse fail
-- this differs from the behaviour for a (Double, Double), where spaces are handled fine.
--
--myl = mkTickLadder' "(0.0,0.001)" "[(1000,1.0), (100,0.1), (10,0.05)]" 

newtype TickSize = MkTickSize Decimal deriving (Show,Eq,Ord)
-- TODO validate the ladder, 
-- 1. all boundaries should themselves be on a ticksize boundary, so that downTick l (uptick l p) == p
data TickLadder = MkTickLadder {
     _ladder :: ([(Price, TickSize)],(Price,TickSize)) -- series of ticksizes, starting price and ticksize, in descending order
   } deriving (Show, Eq, Ord)

-- eg mkTickLadder "(0.0,0.001)" "[(1000,1.0), (100,0.1), (10,0.01)]" 
mkTickLadder :: String -> String -> TickLadder
mkTickLadder ts ls = MkTickLadder (ladder, start)
  where 
    start = (\(x,y) -> (MkPrice x, MkTickSize y)) (read ts :: (Decimal,Decimal))
    ladder = reverse $ sort $ (map (\(x, y) -> (MkPrice x, MkTickSize y)) (read ls::[(Decimal,Decimal)]))

upTick :: TickLadder -> Price -> Price
upTick l p = addPriceTickSize p ts 
  where 
    (_,ts) = rangeBase l p

downTick :: TickLadder -> Price -> Price
downTick l p = subPriceTickSize p ts
  where 
    (_, ts) =rangeBase' l p

-- TODO this could be more efficient, do a binary search on the range
isValidPrice :: TickLadder -> Price -> Bool
isValidPrice l p = elem p (enumLadder l (MkPrice 0) (upTick l p))
 
addPriceTickSize :: Price -> TickSize -> Price
addPriceTickSize (MkPrice p) (MkTickSize t) = MkPrice (p + t)

addPriceTickSize' :: Price -> TickSize -> Int -> Price
addPriceTickSize' (MkPrice p) (MkTickSize t) n = MkPrice (p + (t * (fromIntegral n)))

subPriceTickSize :: Price -> TickSize -> Price
subPriceTickSize (MkPrice p) (MkTickSize t) = MkPrice (p - t)


rangeBase :: TickLadder -> Price -> (Price,TickSize)
rangeBase (MkTickLadder (ls,s)) p = 
  case find ls p of
    Just x -> x
    Nothing -> s

rangeBase' :: TickLadder -> Price -> (Price,TickSize)
rangeBase' (MkTickLadder (ls, s)) p = 
  case find' ls p of
    Just x -> x
    Nothing -> s

find :: [(Price,TickSize)] -> Price -> Maybe (Price,TickSize)
find ls x = case dropWhile (\(p,_) -> x < p) ls of 
              [] -> Nothing 
              (p,t):_ -> Just (p,t)

find' :: [(Price,TickSize)] -> Price -> Maybe (Price,TickSize)
find' ls x = case dropWhile (\(p,_) -> x <= p) ls of 
              [] -> Nothing 
              (p,t):_ -> Just (p,t)

enumLadder :: TickLadder -> Price -> Price -> [Price]
enumLadder tl start end  = takeWhile (\p -> p<=end) $ dropWhile (\p -> p < start) $ enum' ls 
  where 
    -- the ladder in increasing order dropping the ranges below the start price
    ls = dropWhileNext (\(p,_) -> p <= start) l --  map fst $  dropWhile (\((_,_),(p,_)) -> p < start) $ zip l (tail l)
    l = toList tl

enum' :: [(Price, TickSize)] -> [Price]
enum' [] = []
enum' ((_,_):[]) = []
enum' ((ps, ts):(pe, t):ls) = r ++ enum' ((pe,t):ls)
  where
    r = takeWhile (\x -> x <= pe) $ map (addPriceTickSize' ps ts) [0..]

toList :: TickLadder -> [(Price,TickSize)]
toList (MkTickLadder (l,s)) = s : (sort l)

    
dropWhileNext :: (a->Bool) -> [a] -> [a]
dropWhileNext f l = map fst $ dropWhile (\(_,y) -> f y)  $ zip l (tail l)


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

