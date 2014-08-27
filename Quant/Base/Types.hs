{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- TODO: reduce what we export here
--
module Quant.Base.Types
(
  Amount(..)
, Price(..)
, Qty(..)
, AmountMaker(..)
, Side(..)
, Count(..)
, getPrice
, getQty
, getCount
)
where

import qualified Data.Attoparsec.Char8 as P
import qualified Data.ByteString.Char8 as BS
import           Quant.Decimal

newtype Price = MkPrice Decimal deriving (Show,Eq,Ord,Num)
newtype Qty = MkQty Decimal deriving (Show,Eq,Ord,Num)
newtype Amount = MkAmount Decimal deriving (Show,Eq,Ord,Num)
newtype Count = MkCount Decimal deriving (Show,Eq,Ord,Num)

data Side = Buy | Sell deriving (Eq, Show)

class AmountMaker a b where
  toAmount :: a -> b -> Amount

instance AmountMaker Price Qty where
  toAmount (MkPrice p) (MkQty q) = MkAmount (p*q)

instance AmountMaker Amount Qty where
  toAmount (MkAmount p) (MkQty q) = MkAmount (p*q)

-- functions to parse basic types from bytestrings

getPrice :: BS.ByteString -> Price
getPrice b = MkPrice $ parseDecimal b

getQty :: BS.ByteString -> Qty
getQty b = MkQty $ parseDecimal b

getCount :: BS.ByteString -> Count
getCount b = MkCount $ parseDecimal b

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

