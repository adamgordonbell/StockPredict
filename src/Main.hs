{-# LANGUAGE RankNTypes #-}
module Main where

import           Control.Applicative        (Applicative, pure, (<$>), (<*),
                                             (<*>), (<|>))
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Csv
import qualified Data.Vector                as V
import           Network.HTTP.Conduit       (simpleHttp)
import           Types

main :: IO ()
main = do
        putStrLn "Dumb Stock Predictor."
        putStrLn "Grab Historical data and current price on stock from yahoo finance and Buy or sell based on a moving average of close prices"
        putStrLn "Enter Stock Symbol: (ie. YHOO)"
        s <- getLine
        rs <- getHistoricalData s
        c <- getCurrentData s
        print $ "Current Price: " ++ (show c)
        print $ "Last 3 close prices: " ++ (show $ take1 3 rs)
        print $ predict c (take1 3 rs)
  where
        take1 x rs = fmap close . V.toList $ V.take x rs

getHistoricalData :: String -> IO (V.Vector HistoricalRow)
getHistoricalData stock = do
    csvData <- simpleHttp  $ "http://ichart.finance.yahoo.com/table.csv?s=" ++ stock
    let row = decodeByName csvData
    return $ unwrap row

getCurrentData :: String -> IO Double
getCurrentData stock = do
    csvData <- simpleHttp $ "http://download.finance.yahoo.com/d/quotes.csv?f=l1&e=.csv&s=" ++ stock
    return . read $ BL8.unpack csvData

predict :: Double -> [Double] -> Action
predict currentPrice previousPrice
    | last (movavg 3 previousPrice) > currentPrice = Buy
    | otherwise = Sell

movavg :: Int -> [Double] -> [Double]
movavg _ []     = []
movavg n (x:xs) = map (/ n') sums
    where
        sums = scanl (+) (n' * x) $ zipWith (-) xs (replicate n x ++ xs)
        n'   = fromIntegral n

unwrap :: Either t (t1, V.Vector a) -> V.Vector a
unwrap r = case r of
      Left _ ->  V.empty
      Right (_, v) ->  v
