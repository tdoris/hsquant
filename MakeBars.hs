{-# LANGUAGE ScopedTypeVariables,RecordWildCards,OverloadedStrings #-}

import System.Environment
import System.FilePath
import System.Directory
import qualified Data.ByteString.Lazy as BL
import qualified Codec.Compression.BZip as BZip
import qualified Data.ByteString.Char8 as BS

import Quant

--usage: MakeBars "/data/trth" "/data/tdoris/bars/" VOD.L 20120214 
--
main :: IO ()
main = do
  x <- getArgs
  print x
  [ticRoot, outputRoot, ric, dateString] <- getArgs
  let date = parseDate (BS.pack dateString)
  makeBars ticRoot date ric outputRoot

makeBars :: FilePath -> Date -> RIC -> FilePath -> IO ()
makeBars ticRoot date ric outputRoot = do
  quotes <- loadQuotes ticRoot date ric
  trades <- loadTrades ticRoot date ric
  let bars = createBars quotes trades 1 
  let escapedRIC = escapeRIC ric
  let outputDirectory = outputRoot </> dateYear date </> dateMonth date </> dateDay date
  let outputFile = outputDirectory </> escapedRIC <.> "csv" <.> "bz"
  createDirectoryIfMissing True outputDirectory
  --content <- fmap (BS.concat . BL.toChunks . BZip.compress) (BL.readFile f)
  BL.writeFile outputFile (BZip.compress $ toLazy $ BS.unlines $ barCSVHeader : map bar2CSVRow bars)
  print ("wrote " ++ show (length bars) ++ " bars "++ outputFile )

toLazy :: BS.ByteString -> BL.ByteString
toLazy s = BL.fromChunks [s]

fromLazy :: BL.ByteString -> BS.ByteString
fromLazy l = BS.concat $ BL.toChunks l
