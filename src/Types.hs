{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Types where

import Data.Word
import Data.Time
import Data.Text as T
import Data.Data 
import Data.Generics 

-----------------------
-------- TYPES --------
-----------------------


data Action = Buy | Sell | Stay deriving (Show, Enum, Read)

data Symbol = Symbol String deriving (Show,Read)

data Row = Row 

-- Date,Open,High,Low,Close,Volume,Adj Close
-- 2014-09-05,39.05,39.80,39.05,39.59,25808300,39.59