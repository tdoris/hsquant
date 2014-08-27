{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Quant.Base.CSV
(
 CSVTable
, Field
, ColumnName
, loadCSV
, loadCSVBZ
, loadCSVGZ
, parseCSV
, rowCount
, colCount
, columnNames
, getColumn
, getColumns
, getColumnAs
, getColumnAsRead
, column2VectorAs
, column2VectorAsRead
, allRowsSameLength
, removeEmptyRows
, bar2CSVRow
, barCSVHeader
, quote2CSVRow
, trade2CSVRow
)
where

import qualified Codec.Compression.BZip as BZip
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Char8  as BS
import qualified Data.ByteString.Lazy   as BL
import           Data.List
import qualified Data.Vector            as V

import           Quant.Base.Bars
import           Quant.Base.Exchange
import           Quant.Base.DateTime

type ColumnName = String
type Field = BS.ByteString

-- | Handle to CSV data
data CSVTable = CSVTable
  {
    csvTableHeader:: [(ColumnName, Int)]
  , csvTableBody   :: [[Field]]
  , csvTableBodyT  :: [[Field]]
  }

-- | load csv data
loadCSV :: FilePath -> IO CSVTable
loadCSV f = do
  content <- BS.readFile f
  return $! parseCSV content

-- | load csv data from a file that is gzipped
--
-- > loadCSVGZ "test.csv.gz"
loadCSVGZ :: FilePath -> IO CSVTable
loadCSVGZ f = do
  content <- fmap (BS.concat . BL.toChunks . GZip.decompress) (BL.readFile f)
  return $! parseCSV content
-- | load csv data from a file that is bzipped
--
-- > loadCSVBZ "test.csv.bz"
loadCSVBZ :: FilePath -> IO CSVTable
loadCSVBZ f = do
  content <- fmap (BS.concat . BL.toChunks . BZip.decompress) (BL.readFile f)
  return $! parseCSV content

-- | parse csv in a bytestring, assumes the first row is a header line naming the columns
--
-- > fmap parseCSV (B.readFile "the.csv")
--
parseCSV :: BS.ByteString -> CSVTable
parseCSV content = CSVTable colNames ds (transpose ds)
  where
    (header:ds) = map (BS.split ',') $ BS.lines content
    colNames = zip (map BS.unpack header) [0..]

-- | column names
columnNames :: CSVTable -> [String]
columnNames (CSVTable h _ _) = map fst h

rowCount :: CSVTable -> Int
rowCount (CSVTable _ b _) = length b

colCount :: CSVTable -> Int
colCount (CSVTable h _ _) = length h

allRowsSameLength :: CSVTable -> Bool
allRowsSameLength CSVTable{..} = all (== length csvTableHeader) (map length csvTableBody)

removeEmptyRows :: CSVTable -> CSVTable
removeEmptyRows (CSVTable h b _) = CSVTable h b' (transpose b')
  where
    b' = filter (\x -> x /= [] && x /= [BS.empty]) b

-- | retrieve a column by name
--
-- > getColumn table "RIC" == ["VOD.L",..
getColumn :: CSVTable -> ColumnName -> [Field]
getColumn (CSVTable h _ bt) columnName=
  case lookup columnName h of
    Nothing -> error ("column "++columnName ++ " not found")
    Just idx -> bt !! idx

-- | retrieve a group of columns
getColumns :: CSVTable -> [ColumnName] -> [[Field]]
getColumns table names = map (getColumn table) names

-- | retrieve a column and convert it to a type
--
-- > getColumnAs table "Price" (ByteString.Lexing.readDouble) == [123.2,..
getColumnAs :: CSVTable -> ColumnName -> (BS.ByteString -> a) -> [a]
getColumnAs table name convert = map convert (getColumn table name)

-- | retrieve a column and convert it to a type using Read - this allows you to just specify the type you'd like
--   the column to be with a type annotation or implicitly via type inference
--   and if that type has an instance of the Read typeclass the conversion is done for you,
--   for example:
--
-- > getColumnAsRead table "Price" :: [Double]
getColumnAsRead :: Read a => CSVTable -> ColumnName -> [a]
getColumnAsRead table name = map read $ map BS.unpack (getColumn table name)

-- | retrieve a column as a vector using a supplied function to convert from ByteString to a type
column2VectorAs :: CSVTable -> ColumnName -> (BS.ByteString -> a) -> V.Vector a
column2VectorAs table name convert = V.fromList $ getColumnAs table name convert

-- | retrieve a column as a vector using Read to convert types
column2VectorAsRead :: Read a => CSVTable -> ColumnName -> V.Vector a
column2VectorAsRead table name = V.fromList $ getColumnAsRead table name

bar2CSVRow :: Bar -> BS.ByteString
bar2CSVRow Bar{..} = BS.intercalate "," $ map BS.pack [toTimeStamp barTime, show barOpenBid, show barLowBid, show barHighBid, show barCloseBid, show barOpenAsk, show barLowAsk, show barHighAsk, show barCloseAsk, show barTWABidQty, show barTWAAskQty, show barVWAP, show barVolume, show barTradedBidQty, show barTradedAskQty, show barTradedUnknownQty]

barCSVHeader :: BS.ByteString
barCSVHeader = BS.pack "time, bidopen, bidlow, bidhigh, bidclose, askopen, asklow, askhigh, askclose, bidqtytwa, askqtytwa, vwap, volume, bidqtytraded, askqtytraded, unknownqtytraded"

trade2CSVRow :: ExchangeTrade -> BS.ByteString
trade2CSVRow ExchangeTrade{..} = BS.intercalate "," [BS.pack $ toTimeStamp tradeTime, BS.pack $ show tradePrice, BS.pack $ show tradeQty]

quote2CSVRow :: ExchangeQuote -> BS.ByteString
quote2CSVRow ExchangeQuote{..} = BS.intercalate "," [BS.pack $ toTimeStamp quoteTime, BS.pack $ show quoteBidQty, BS.pack $ show quoteBid, BS.pack $ show quoteAsk, BS.pack $ show quoteAskQty]

toTimeStamp :: TimeOfDay -> String
toTimeStamp (TimeOfDay t) = intercalate ":" [hourString, minString, secString] ++ "." ++ msecString
  where
    hourString = if hour < 10 then '0' : show hour else show hour
    minString = if minute <10 then '0': show minute else show minute
    secString = if sec <10 then '0' : show sec else show sec
    msecString | msec < 10  = "00"++show msec
               | msec < 100 = '0' : show msec
               | otherwise  = show msec
    hour = t `div` 3600000
    minute = (t - hour*3600000) `div` 60000
    sec = (t - hour*3600000 - minute* 60000) `div` 1000
    msec = t `mod` 1000

