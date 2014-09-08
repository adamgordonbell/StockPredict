{-# LANGUAGE RankNTypes #-}
module Main where

import           Control.Applicative
import           Control.Monad.IO.Class  (liftIO)
-- import           Data
import           Types
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

import Data.Conduit
import Network.HTTP.Conduit
import Data.Conduit.Binary (sinkFile)
import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import Data.Time
-- import Database.PostgreSQL.Simple.FromField


import           Control.Applicative
                   ( Applicative, (<|>), (<$>), (<*>), (<*), pure )
import           Control.Exception (SomeException(..), Exception)
import qualified Data.Attoparsec.Char8 as Atto
import qualified Data.Attoparsec.Combinator as Atto
-- import Data.Attoparsec.Types (Parser)
import Data.Attoparsec.Char8 (double, parseOnly)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

main2 :: IO ()
main2 = do
        print "Enter Stock Symbol: (ie. YHOO)"
        s <- getLine
        getData s
        print $ predict s

movavg n []     = []
movavg n (x:xs) = map (/ n') sums
    where
        sums = scanl (+) (n' * x) $ zipWith (-) xs (replicate n x ++ xs)
        n'   = fromIntegral n

predict :: String -> Action
predict _ = predict1 3.0 [2.1,2.3,2.4]

predict1 :: Float -> [Float] -> Action
predict1 currentPrice previousPrice 
    | (last $ movavg 3 previousPrice) > currentPrice = Buy
    | otherwise = Sell

getData :: [Char] -> IO ()
getData stock = do
    r <- simpleHttp  $ "http://ichart.finance.yahoo.com/table.csv?s=" ++ stock
    print r


instance FromField Day where
    parseField f = parseDate f


dateParser :: Atto.Parser Day
dateParser = do
  y  <- Atto.count 4 Atto.digit
  Atto.char '-'
  mm <- Atto.count 2 Atto.digit
  Atto.char '-'
  d  <- Atto.count 2 Atto.digit
  return $
    fromGregorian (read y) (read mm) (read d)

parseDate :: B.ByteString -> Parser Day
parseDate s = case parseOnly dateParser s of
    Left err -> typeError "Day" s (Just err)
    Right n  -> pure n


data Person = Person
    { name1   :: !String
    , salary :: !Int
    , date   :: !Day
    }

instance FromNamedRecord Person where
    parseNamedRecord r = Person <$> r .: "name" <*> r .: "salary" <*> r .: "date"

typeError :: String -> B.ByteString -> Maybe String -> Parser a
typeError typ s mmsg =
    fail $ "expected " ++ typ ++ ", got " ++ show (B8.unpack s) ++ cause
  where
    cause = case mmsg of
        Just msg -> " (" ++ msg ++ ")"
        Nothing  -> ""

main :: IO ()
main = do
    csvData <- BL.readFile "data/salaries.csv"
    case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, v) -> V.forM_ v $ \ p ->
            putStrLn $ name1 p ++ " earns " ++ show (salary p) ++ " dollars " ++ show(date p)  ++ " date"

