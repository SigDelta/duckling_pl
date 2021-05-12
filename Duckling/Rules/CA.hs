-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}


module Duckling.Rules.CA
  ( defaultRules
  , langRules
  , localeRules
  ) where


import Duckling.Dimensions.Types
import Duckling.Locale
import qualified Duckling.AmountOfMoney.CA.Rules as AmountOfMoney
import qualified Duckling.Numeral.CA.Rules as Numeral
import qualified Duckling.Ordinal.CA.Rules as Ordinal
import Duckling.Types

defaultRules :: Seal Dimension -> [Rule]
defaultRules = langRules

localeRules :: Region -> Seal Dimension -> [Rule]
localeRules region (Seal (CustomDimension dim)) = dimLocaleRules region dim
localeRules _ _ = []

langRules :: Seal Dimension -> [Rule]
langRules (Seal AmountOfMoney) = AmountOfMoney.rules
langRules (Seal CreditCardNumber) = []
langRules (Seal Distance) = []
langRules (Seal Duration) = []
langRules (Seal Email) = []
langRules (Seal Numeral) = Numeral.rules
langRules (Seal Ordinal) = Ordinal.rules
langRules (Seal PhoneNumber) = []
langRules (Seal Quantity) = []
langRules (Seal RegexMatch) = []
langRules (Seal Temperature) = []
langRules (Seal Time) = []
langRules (Seal TimeGrain) = []
langRules (Seal Url) = []
langRules (Seal Volume) = []
langRules (Seal (CustomDimension _)) = []
