{-# LANGUAGE ScopedTypeVariables,RecordWildCards #-}

import System.Environment
import Text.CSV
import Data.List
import Graphics.Plot
import Numeric.Container
import Numeric.GSL
import Numeric.GSL.Statistics
import qualified Data.Map as M

import Quant.Base.Types
import Quant.Base.DataAccess
-- usage should be:
-- Calibrate RIC StartDate DayCount
--
main = do
  [ricArg, startDate, dayCountArg] <- getArgs
  bars <- loadBars root date ric
--  writeFile "/tmp/cleanedbars" (show bars)
  let tradeSizes = [10000, 50000, 100000, 250000,500000, 1000000]
  let icurve = calcImpactCurve bars tradeSizes 
  print icurve
  mplot [fromList tradeSizes, fromList icurve]

data Time = Time Integer deriving (Show,Eq)

type Ratio = Double
type RatioPct = Int 
ratio2RatioPct :: Ratio -> RatioPct
ratio2RatioPct r = floor (r * 100.0)

bMid Bar{..} = (bStartBid + bStartAsk) / 2.0
bRatio Bar{..} = bVolumeUp / (bVolumeUp + bVolumeDn)

calcImpactCurve :: [Bar] -> [VolumeUp] -> [Double]
calcImpactCurve bars vups = map (impact bars returnMap) vups
  where
    returnMap = expectedReturnMap bars ratioBounds
    ratioBounds = [0.01,0.02..1.0]

impact :: [Bar] -> M.Map RatioPct Return -> VolumeUp -> Double
impact bars returnTable vup = mean $ fromList $ map (lookupExpectedReturn returnTable) $ map (ratio2RatioPct . (ratioNew vup)) bars
   
expectedReturnMap :: [Bar] -> [Ratio] -> M.Map RatioPct Return
expectedReturnMap bars ratios = M.fromList $ zip (map ratio2RatioPct ratios) (map (mean . fromList . rets) barss) 
  where
    ranges = zip ratios (tail ratios)
    barss = map (\(l,u)-> filter (\b -> bRatio b < u && bRatio b > l) bars) ranges

between :: (a -> Double) -> (Double, Double) -> a -> Bool
between f (l,u) x = l < f x && f x < u 

lookupExpectedReturn :: M.Map RatioPct Return -> RatioPct -> Return
lookupExpectedReturn rrs ratio = 
  case M.lookup ratio rrs of 
    Nothing -> 0.0
    Just x -> x

ratioNew :: VolumeUp -> Bar -> Ratio
ratioNew vup' Bar{..} = (bVolumeUp+vup') / (bVolumeUp + vup'+ bVolumeDn)

percentile :: Double -> [Double] -> Double
percentile p ds = quantile p $ fromList $ sort ds 

addReturns :: [Bar] -> [Bar]
addReturns bars = barsWithT1 
  where 
    barsWithT0 = map (\(b1,b2) -> b1{ bReturnT0 = (bMid b2 - bMid b1) / bMid b1 }) $ zip bars (tail bars)
    barsWithT1 = map (\(b1,b2) -> b1{ bReturnT1 = bReturnT0 b2 }) $ zip barsWithT0 (tail barsWithT0)

cleanbars :: [Bar] -> [Bar]
cleanbars = filter (\Bar{..} -> bVolumeUp /= 0 && bVolumeDn /= 0) 

rets :: [Bar] -> [Double]
rets bars =  map bReturnT0 bars

retsT1 :: [Bar] -> [Double]
retsT1 bars = map bReturnT1 bars

fromRow :: [String] -> Bar
fromRow row@([time,volume,start_bid,start_ask,volume_up,volume_dn,volume_neither,trades,quotes]) = 
  Bar (ptime time) (read start_bid) (read start_ask) (read volume_up) (read volume_dn) (0.0) (0.0)
fromRow x = error ("fromRow, cannot parse:"++show x)

ptime :: String -> Time
ptime [h1,h2,':',m1,m2,':',s1,s2,'.',ms1,ms2,ms3] = Time ((read [h1,h2])*3600000 + (read [m1,m2])*60000 + (read [s1,s2])*1000 + (read [ms1,ms2,ms3]))
ptime t = error ("Unrecognised time format:"++t)

meanDeltaRatio :: Double -> [Bar] -> Double
meanDeltaRatio vup bars = mean $ fromList deltaRatios 
  where 
    oldRatios = map bRatio bars
    newRatios = map (ratioNew vup) bars
    deltaRatios = zipWith (\x y -> x - y) newRatios oldRatios 

scratch :: [Bar] -> [Double] -> IO ()
scratch bars tradeSizes = do
  let groups = [0.0,0.05..1.0]
  let ranges = zip groups (tail groups)
  let z = map (\(lbRatio, ubRatio) bar -> bRatio bar < ubRatio && bRatio bar > lbRatio) (zip groups (tail groups))
  let barss = map (\(l,u)-> filter (\b -> bRatio b < u && bRatio b > l) bars) ranges
   
  let rc = map (mean . fromList . rets) barss 
  let ub = map (percentile 0.95 . rets) barss 
  let lb = map (percentile 0.05 . rets) barss
  let ub2 = map (percentile 0.90 . rets) barss
  let lb2 = map (percentile 0.10 . rets) barss
  print ("return curve:" ++ show rc)
  let midPoints = zipWith (\a b -> (a + b) / 2.0) groups (tail groups)
  let returnMap = expectedReturnMap bars [0.01,0.02..1.0]
  let impactCurve = map (impact bars returnMap) tradeSizes 
  
  mplot [fromList midPoints, fromList rc, fromList ub, fromList lb, fromList ub2, fromList lb2]
  mplot [fromList tradeSizes, fromList impactCurve]
  print ("lag1 autocorrelation of ratio:" ++ show (lag1auto (fromList (map bRatio bars))))
  print "Complete"

