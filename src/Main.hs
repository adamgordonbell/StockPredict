{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
module Main where

import           Control.Applicative        (Applicative, pure, (<$>), (<*),
                                             (<*>), (<|>))
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Csv
import qualified Data.Vector                as V
import           Network.HTTP.Conduit       (simpleHttp)
import           Numeric.LinearAlgebra
import           Types

type HistoricalData = V.Vector HistoricalRow

main :: IO ()
main = do
        putStrLn "Dumb Stock Predictor."
        putStrLn "Grab Historical data and current price on stock from yahoo finance and Buy or sell based on a moving average of close prices"
        putStrLn "Enter Stock Symbol(s) seperated by space: (ie. YHOO GOOG)"
        s <- getLine
        d <- getStockDataMany $ words s
        let rs = predictMany d
        print rs
      where
        predictMany = fmap (uncurry predictMovingAvg)

getStockDataMany :: [String] -> IO [(HistoricalData, Double)]
getStockDataMany = sequence . fmap getStockData

getStockData :: String -> IO((HistoricalData,Double))
getStockData stock = do
        rs <- getHistoricalData stock
        c <- getCurrentData stock
        return (rs,c)

getHistoricalData :: String -> IO (HistoricalData)
getHistoricalData stock = do
    putStrLn $ "Getting " ++ stock ++ " historical stock data"
    csvData <- simpleHttp  $ "http://ichart.finance.yahoo.com/table.csv?s=" ++ stock
    let row = decodeByName csvData
    return $ unwrap row

getCurrentData :: String -> IO Double
getCurrentData stock = do
    putStrLn $ "Getting " ++ stock ++ " current price"
    csvData <- simpleHttp $ "http://download.finance.yahoo.com/d/quotes.csv?f=l1&e=.csv&s=" ++ stock
    return . read $ BL8.unpack csvData

predictMovingAvg :: HistoricalData -> Double -> Action
predictMovingAvg hData currentPrice
    | last (movavg 3 dataUnwrapped) > currentPrice = Buy
    | otherwise = Sell
   where dataUnwrapped = fmap close . V.toList . V.take 3 $ hData

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

-------------------------------------------
-- Perform Linear regression and use slope to predict stock price
------------------------------------------
regression :: V.Vector Double -> Double
regression m1 = (toLists (linearSolveLS r1 m)) !! 0 !! 0
        where
             r1 = fromList $ V.toList m1

m :: Matrix Double
m = undefined
