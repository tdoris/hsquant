module Quant
(
  module Quant.Base.Types
, module Quant.Base.DataAccess
, module Quant.Base.Utils
, module Quant.Base.Bars
, module Quant.Base.CSV
, module Quant.Base.KTime
)
where

{-
TODO:

restructure so that there's some order to the modules

basic types Price, Amount, Qty (done)
exchange types Quote, Trade etc (done, now in Quant.Base.Exchange)
time and dates
timeseries
basic operations on timeseries
data access
fix KTime
regressions and basic statistics (utils currently)

perhaps things like bars should separate modules

-}

import           Quant.Base.Bars
import           Quant.Base.CSV
import           Quant.Base.DataAccess
import           Quant.Base.KTime
import           Quant.Base.Types
import           Quant.Base.Utils
