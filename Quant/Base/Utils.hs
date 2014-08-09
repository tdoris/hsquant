-- some convenience utility functions for quant analysis. A lot of these
-- are simple wrappers around the Statistics library functions, in part, the
-- purpose of this module is to document how to call the library functions to get
-- a sensible, expected default, an exercise that often involves figuring out which
-- of several packages should be used (e.g. the gsl wrappers are comprehensive and but
-- I hit some bugs (segfaults) when running under GHCi on 64-bit linux that don't look like
-- they'll be fixed soon). We also intentionally provide versions that
-- operate on [Double] rather than Vector Double for convenience. For
-- higher performance (iff you have vectors with >> 1000 elements)
-- you should get the data into Vectors and use the Vector
-- functions directly

module Quant.Base.Utils
(
  mean
, variance
, sd
, moment
, quantile
, Summary (..)
, summary
, tstat
, cumsum
, cumprod
, cummin
, cummax
, rnorm
{-, plot
, plotHist
, plotKDE
-}
, bucket
, dropUntilNext
, takeWhilePlusOne
, dropWhileLessOne
)
where

import           Control.Monad
import           Data.List
import qualified Data.Vector.Storable            as V
import qualified Data.Vector.Unboxed             as UV
--import qualified Graphics.Plot                   as Plot
import qualified Statistics.Quantile             as Quantile
import qualified Statistics.Sample               as Sample
import qualified Statistics.Sample.Histogram     as H
import qualified Statistics.Sample.KernelDensity as KernelDensity
import           System.Random.MWC
import           System.Random.MWC.Distributions

mean :: [Double] -> Double
mean ds = Sample.mean (V.fromList ds)

-- uses Sample.variance, consider Sample.varianceUnbiased
variance :: [Double] -> Double
variance = Sample.variance . V.fromList

sd :: [Double] -> Double
sd = Sample.stdDev . V.fromList

moment :: Int -> [Double] -> Double
moment k d = Sample.centralMoment k (V.fromList d)

-- uses the continuousBy and "s" method to match R defaults
quantile :: Int -- ^ /k/, the desired quantile
        -> Int -- ^ /q/, the number of quantiles
        -> [Double] -- /x/, the sample data
        -> Double
quantile k qCount ds = Quantile.continuousBy Quantile.s k qCount (V.fromList ds)

data Summary = Summary
  {
    summMin    :: Double
  , summ1stQ   :: Double
  , summMedian :: Double
  , summMean   :: Double
  , summ3rdQ   :: Double
  , summMax    :: Double
  } deriving Show

summary :: [Double] -> Summary
summary ds = Summary (minimum ds) (quantile 1 4 ds) (quantile 2 4 ds) (mean ds) (quantile 3 4 ds) (maximum ds)

tstat :: [Double] -> Double
tstat ds = (mean ds) / ((sd ds) / sqrt n)
  where n = genericLength ds

cumsum :: Num a => [a]->[a]
cumsum [] = error "cumsum called on empty list"
cumsum s = scanl1 (+) s

cumprod :: Num a => [a]->[a]
cumprod [] = error "cumprod called on empty list"
cumprod s = scanl1 (*) s

cummax :: Ord a => [a]->[a]
cummax [] = error "cummax called on empty list"
cummax s = scanl1 max s

cummin :: Ord a => [a]->[a]
cummin [] = error "cummin called on empty list"
cummin s = scanl1 min s

-- plot wrapper function
-- latest versions of hmatrix don't include plotting, wait until chris done's package isready
{--
--plot :: [[Double]] -> IO ()
--plot xs = Plot.mplot (map V.fromList xs)

-- TODO - not sure why kde uses Unboxed vectors, the rest of the Statistics package seems to
-- use Generic
plotKDE :: [Double] -> IO ()
plotKDE ds = plot [UV.toList xs, UV.toList ys]
  where (xs, ys) = KernelDensity.kde 32 (UV.fromList ds)
plotHist :: [Double] -> IO ()
plotHist ds = plot [V.toList xs, V.toList ys]
  where (xs, ys) = H.histogram 32 (V.fromList ds)
-}

-- regression
--
lm :: [Double] -> [Double] -> (Double, Double)
lm ys xs =  undefined
  where (alpha, beta) = undefined

-- some helper functions for generating random numbers, cos you'll never remember the withSystemRandom asGenIO crud

-- | rnorm count mean sd
rnorm :: Int -> Double -> Double -> IO [Double]
rnorm count mean sd = withSystemRandom . asGenIO $ \gen -> replicateM count (normal mean sd gen)

-- | 'bucket' creates a sublist containing elements that satisfy each predicate in turn
--   For example:
--
--   > bucket [(<10),(<20)] [5,10,15] == [[5],[10,15]]
bucket :: [a->Bool] -> [a] -> [[a]]
bucket (p:ps) ls = takeWhile p ls : bucket ps (dropWhile p ls)
bucket _ _ = []

-- | drop elements until the next element satisfies the predicate
dropUntilNext :: (a->Bool) -> [a] -> [a]
dropUntilNext p ls = (take 1 $ reverse $ takeWhile (not . p) ls) ++  dropWhile (not . p) ls

-- | take one more element than is satisfied by the predicate
takeWhilePlusOne :: (a -> Bool) -> [a] -> [a]
takeWhilePlusOne p ls = takeWhile p ls ++ (take 1 $ dropWhile p ls)

-- | drop until the next element does not satisfy the predicate
dropWhileLessOne :: (a -> Bool) -> [a] -> [a]
dropWhileLessOne p ls = (take 1 $ reverse $ takeWhile p ls) ++ dropWhile p ls
