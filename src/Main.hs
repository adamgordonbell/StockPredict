module Main where

import           Control.Applicative
import           Control.Monad.IO.Class  (liftIO)
import           Data
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH


main :: IO ()
main = do
        print "Enter Stock Symbol"
        s <- Symbol <$> getLine
        print $ predict s

movavg n []     = []
movavg n (x:xs) = map (/ n') sums
    where
        sums = scanl (+) (n' * x) $ zipWith (-) xs (replicate n x ++ xs)
        n'   = fromIntegral n

data Action = Buy | Sell | Stay deriving (Show, Enum, Read)

data Symbol = Symbol String deriving (Show,Read)

predict :: Symbol -> Action
predict _ = Buy



--  http://ichart.finance.yahoo.com/table.csv?s=YHOO
