{-# LANGUAGE RecordWildCards, OverloadedStrings, TypeSynonymInstances,DeriveDataTypeable,MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
module Quant.Base.KTime (
  KTime (..)
, Period (..)
, getKTimeNow
, ktimeToString
, getKTimeDayStart
, getKTimeDayStartOffset 
, convert
, diffKTimeSeconds 
, ktimeToUTC 
, formatKTimeYMDHMS 
, formatKTimeYMD
)
where

import GHC.Generics (Generic)
import Data.Data
import Data.Convertible
import Data.Time
import Data.Time.Clock.POSIX

-- milliseconds since the unix epoch always in UTC 
-- ktime = 1000 * posix_time
data KTime = KTime Integer deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

instance Convertible KTime Integer where
  safeConvert (KTime k) = return k
instance Convertible KTime UTCTime where
  safeConvert k = return (ktimeToUTC k)
instance Convertible UTCTime KTime where
  safeConvert u = return (utcToKTime u)

data Period = Period KTime KTime deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

getKTimeNow :: IO KTime
getKTimeNow = do 
  t <- getPOSIXTime
  return (KTime (1000*(convert t :: Integer)))

ktimeToString :: KTime -> String
ktimeToString (KTime k) = 
  show $ zonedTimeToLocalTime (utcToZonedTime utc (posixSecondsToUTCTime (fromIntegral k / 1000)))

ktimeToUTC :: KTime -> UTCTime 
ktimeToUTC (KTime k) = posixSecondsToUTCTime (fromIntegral k / 1000)

utcToKTime :: UTCTime -> KTime
utcToKTime u = KTime ( convert u :: Integer)

millisInDay :: Integer
millisInDay = 24*60*60*1000

getKTimeDayStart :: KTime -> KTime 
getKTimeDayStart = getKTimeDayStartOffset 0  

getKTimeDayStartOffset :: Integer -> KTime ->  KTime
getKTimeDayStartOffset dayOffset (KTime k) = KTime ((k - (k `mod` millisInDay)) + (dayOffset*millisInDay))

diffKTime :: KTime -> KTime -> Integer
diffKTime (KTime a) (KTime b) = b - a

diffKTimeSeconds :: KTime -> KTime -> Integer
diffKTimeSeconds a b = k `div` 1000
 where k = diffKTime a b 

formatKTimeYMDHMS :: KTime -> String
formatKTimeYMDHMS time = formatTime defaultTimeLocale "%Y%m%d_%H_%M_%S" $ (ktimeToUTC time)

formatKTimeYMD :: KTime -> String
formatKTimeYMD time = formatTime defaultTimeLocale "%Y%m%d" $ (ktimeToUTC time)
--getDaysOfPeriod :: Period -> [KTime]
--getDaysOfPeriod (Period (KTime start) (KTime end)) = map (getKTimeDayStart . KTime) [start,start+millisInDay..end]

