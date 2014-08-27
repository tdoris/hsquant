{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- TODO: reduce what we export here
--
module Quant.Base.DateTime
(
  TimeStamp
, Date(..)
, Year
, Month
, Day
, TimeOfDay(..)
, parseDate
, parseTime
, getTime
)
where

import           Control.Applicative
import qualified Data.Attoparsec.Char8 as P
import qualified Data.ByteString.Char8 as BS

-- | simplistic representation of time of day as the number of milliseconds since midnight,
--   no assumptions about timezones or conversions
data TimeOfDay = TimeOfDay Integer deriving (Eq,Ord,Show)

type TimeStamp = String

data Date = Date
  { dateYear  :: Year
  , dateMonth :: Month
  , dateDay   :: Day
  } deriving (Eq, Ord, Show)

type Year = String
type Month = String
type Day = String

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


