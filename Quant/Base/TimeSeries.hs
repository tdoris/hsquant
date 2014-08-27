{-# LANGUAGE FlexibleInstances #-}

module Quant.Base.TimeSeries
()
where
import qualified Data.Vector as V
import Data.Time

{-
--class TimeSeries ts t a  where
--  fromList ::  [(t,a)] -> ts
--  toList   ::  ts -> [(t, a)]

data TimeSeries a = MkTimeSeries { _times :: V.Vector UTCTime, _values :: V.Vector a } deriving (Eq, Show)

-- truncate the longer list so the parallel vectors are the same length
mkTimeSeries :: V.Vector UTCTime -> V.Vector a -> TimeSeries a
mkTimeSeries ts as = MkTimeSeries (V.take l ts) (V.take l as)
  where 
    l = min (V.length ts) (V.length as)

--mkTimeSeries' :: Traversible 
--data PVector t a = MkPVector (V.Vector t) (V.Vector a)

--instance TimeSeries (PVector UTCTime Double) where
--  fromList s = MkPVector (V.fromList $ map fst s) (V.fromList $ map snd s)
--  toList (MkPVector ts as) = zip (V.toList ts) (V.toList as)


x :: TimeSeries Double
x = MkTimeSeries (V.fromList []) (V.fromList [])
-}


-- Hugo's suggestions

data TimeSeries v = MkTimeSeries {index :: UTCTime, value :: v} deriving (Eq, Show)
 
----------------------------  Dealing with the box ---------------------------
instance Functor TimeSeries  where
  fmap f (MkTimeSeries i a) = MkTimeSeries i (fmap f a)
{- 
mapTs :: (b->c) -> TimeSeries a [b] -> TimeSeries a [c]
mapTs f (TimeSeries a [b]) = TimeSeries a (map f [b])
 
apply :: (b -> c) -> TimeSeries a b -> TimeSeries a c
apply f (TimeSeries a b) = TimeSeries a (f b)
 
prepend :: a -> b -> TimeSeries [a] [b] -> TimeSeries [a] [b]
prepend a b (TimeSeries a1 b1) = TimeSeries (a:a1) (b:b1)
 
mergeU :: (Ord a) => (b->c->d) -> d -> TimeSeries [a] [b] -> TimeSeries [a] [c] -> TimeSeries [a] [d]
-- to be completed
mergeU f nan (TimeSeries (a0:a1) (b0:b1)) (TimeSeries (c0:c1) (d0:d1))
    | a0 < c0 = prepend a0 nan $ mergeU f nan (TimeSeries a1 b1) (TimeSeries (c0:c1) (d0:d1))
    | a0 > c0 = prepend c0 nan $ mergeU f nan (TimeSeries (a0:a1) (b0:b1)) (TimeSeries c1 d1)
    | a0 == c0 = prepend a0 (f b0 d0) $ mergeU f nan (TimeSeries a1 b1) (TimeSeries c1 d1)
               
mergeI :: (Ord a) => (b->c->d) -> TimeSeries [a] [b] -> TimeSeries [a] [c] -> TimeSeries [a] [d]
mergeI f (TimeSeries [] []) (TimeSeries [] []) = TimeSeries [] []
mergeI f (TimeSeries a b) (TimeSeries [] []) = TimeSeries [] []
mergeI f (TimeSeries [] []) (TimeSeries a b) = TimeSeries [] []
mergeI f (TimeSeries (a0:a1) (b0:b1)) (TimeSeries (c0:c1) (d0:d1))
                | a0 < c0 = mergeI f (TimeSeries a1 b1) (TimeSeries (c0:c1) (d0:d1))
                | a0 > c0 = mergeI f (TimeSeries (a0:a1) (b0:b1)) (TimeSeries c1 d1)
                | a0 == c0 = prepend a0 (f b0 d0) $ mergeI f (TimeSeries a1 b1) (TimeSeries c1 d1)
 
----------------------- Likely functions to apply to series ------------------
 
ewma :: Double -> [Double] -> [Double]
ewma com arg =
    let a = 1.0 - (1.0 / (1.0 + com))
                in ewma_ a 0 0 arg
               
ewma_ :: Double -> Double -> Double -> [Double] -> [Double]
ewma_ mult sum count [] = []
ewma_ mult sum count (a0:at)
    | not (a0==a0) = (sum / count):(ewma_ mult (sum*mult) (count*mult) at)
    | otherwise    = (((sum*mult)+a0)/((count*mult)+1)) : (ewma_ mult ((sum*mult)+a0) ((count*mult)+1) at)
               
 
               
               
------------------------------------ Tests: ----------------------------------
 
ts0 = TimeSeries [0,1,2,3,4,10,20,30] [1.0..8.0] :: TimeSeries [Int] [Double]
ts1 = TimeSeries [0,1,2,3,6,10,20,30] [1.0..8.0] :: TimeSeries [Int] [Double]
-}
 
-- show value mergeI (\x y = x + y) ts0 ts1
-- rollingMean
-- cumSum
-- zScore
-- could try an ewma with the delta -time used too, but I'm not sure if that would be much better
-- fft
-- ifft
-- sinTransform
-- cosTransform
-- autoCorrelation
-- smooth  // 1 2 1  kernel or something
-- convolution
-- lag
-- wavelet transform  // low priority
 

