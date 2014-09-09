{-# LANGUAGE RankNTypes #-}
module Main where

import           Control.Applicative    (Applicative, pure, (<$>), (<*), (<*>),
                                         (<|>))
import           Control.Monad.IO.Class (liftIO)
import           Types

import qualified Data.ByteString.Lazy   as BL (readFile)
import           Data.Conduit.Binary    (sinkFile)
import           Data.Csv               (decodeByName)
import           Data.Csv
import qualified Data.Vector            as V
import           Network.HTTP.Conduit   (simpleHttp)

main :: IO ()
main = do
        print "Enter Stock Symbol: (ie. YHOO)"
        s <- getLine
        rs <- getData1 s
        V.forM_ rs  $ \ r ->
            putStrLn $  show (date r) ++ ": " ++ show (date r)

movavg n []     = []
movavg n (x:xs) = map (/ n') sums
    where
        sums = scanl (+) (n' * x) $ zipWith (-) xs (replicate n x ++ xs)
        n'   = fromIntegral n

predict :: String -> Action
predict _ = predict1 3.0 [2.1,2.3,2.4]

predict1 :: Float -> [Float] -> Action
predict1 currentPrice previousPrice
    | last (movavg 3 previousPrice) > currentPrice = Buy
    | otherwise = Sell

getData1 :: String -> IO (V.Vector Row)
getData1 stock = do
    csvData <- simpleHttp  $ "http://ichart.finance.yahoo.com/table.csv?s=" ++ stock
    let row = decodeByName csvData
    case row of
      Left _ -> return $ V.empty
      Right (_, v) -> return v
