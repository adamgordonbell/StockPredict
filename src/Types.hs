{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
module Types where

import qualified Data.Attoparsec.Char8      as Atto (Parser, char, count, digit,
                                                     double, parseOnly)
import qualified Data.Attoparsec.Combinator as Atto (count)
import           Data.Csv                   (FromField, FromNamedRecord, Parser,
                                             parseField, parseNamedRecord, (.:))
import           Data.Time                  (Day, fromGregorian)

import           Control.Applicative        (Applicative, pure, (<$>), (<*),
                                             (<*>), (<|>))
import qualified Data.ByteString            as B (ByteString)
import qualified Data.ByteString.Char8      as B8 (ByteString, unpack)

-----------------------
-------- TYPES --------
-----------------------


data Action = Buy | Sell | Stay deriving (Show, Enum, Read)

data Symbol = Symbol String deriving (Show,Read)

data Row = Row
    { date    :: !Day
    , open    :: !Double
    , high    :: !Double
    , low     :: !Double
    , close   :: !Double
    , volume  :: !Int
    ,adjClose :: !Double
    }

instance FromNamedRecord Row where
    parseNamedRecord r = Row <$>
                             r .: "Date" <*>
                             r .: "Open" <*>
                             r .: "High" <*>
                             r .: "Low" <*>
                             r .: "Close" <*>
                             r .: "Volume" <*>
                             r .: "Adj Close"

instance FromField Day where
    parseField = parseDate

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
parseDate s = case Atto.parseOnly dateParser s of
    Left err -> typeError "Day" s (Just err)
    Right n  -> pure n


typeError :: String -> B.ByteString -> Maybe String -> Parser a
typeError typ s mmsg =
    fail $ "expected " ++ typ ++ ", got " ++ show (B8.unpack s) ++ cause
  where
    cause = case mmsg of
        Just msg -> " (" ++ msg ++ ")"
        Nothing  -> ""

-- Date,Open,High,Low,Close,Volume,Adj Close
-- 2014-09-05,39.05,39.80,39.05,39.59,25808300,39.59
