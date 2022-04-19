-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.PL.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.AmountOfMoney.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale PL Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple PLN 1)
             [ "1 zł"
             , "jeden złoty"
             , "1 PLN"
             ]
  , examples (simple PLN 10)
             [ "10 złotych"
             , "PLN 10"
             , "10 zł."
             , "10zł"
             , "10 PLN"
             , "10 złotymi"
             ]
  , examples (simple Dollar 1)
             [ "$1"
             , "jeden dolar"
             ]
  , examples (simple Dollar 10)
             [ "$10"
             , "$ 10"
             , "10$"
             , "10 dolarów"
             , "dziesięć dolarów"
             ]
  , examples (simple Cent 10)
             [ "10 centów"
             , "dziesięć centów"
             , "10 c"
             , "10¢"
             ]
  , examples (simple Dollar 1e4)
             [ "$10К"
             , "10k$"
             , "$10,000"
             ]
  , examples (simple USD 3.14)
             [ "USD3.14"
             , "3.14US$"
             , "US$ 3.14"
             ]
  , examples (simple EUR 20)
             [ "20 euro"
             , "20 Euro"
             , "EUR 20"
             , "EUR 20.0"
             , "20€"
             , "20 eur"
             ]
  , examples (simple Pound 10)
             [ "\x00a3\&10"
             , "dziesięć funtów"
             ]
  , examples (simple INR 20)
             [ "Rs. 20"
             , "Rs 20"
             , "20Rs"
             , "Rs20"
             ]
  , examples (simple Dollar 20.43)
             [ "$20 i 43c"
             , "$20 43"
             , "20 dolarów 43 centów"
             , "20 dolarów i 43 centów"
             ]
  , examples (simple GBP 3.01)
             [ "GBP3.01"
             , "GBP 3.01"
             , "3 GBP 1 pens"
             ]
  , examples (simple Unnamed 42)
             [ "42 baksów"
             , "42 monet"
             , "42 zeta"
             ]
  , examples (between Dollar (10, 20))
             [ "Między dziesięć i dwadzieścia dolarów"
             , "Od 10 do 20 dolarów"
             , "10-20 dolarów"
             , "między dziesięcioma i dwudziestoma dolarami"
             , "10$-20$"
             ]
  , examples (under EUR 7)
             [ "mniej niż 7 euro"
             , "poniżej 7-miu euro"
             , "poniżej siedmiu euro"
             , "mniej niż siedem €"
             , "nie więcej niż 7 EUR"
             , "nie więcej od 7 euro"
             ]
  , examples (above Dollar 1.42)
             [ "więcej niż dolar czterdzieści dwa"
             , "minimum $1.42"
             , "więcej od 1.42 dolarów"
             , "powyżej dolara i 42 centów"
             ]
  ]
