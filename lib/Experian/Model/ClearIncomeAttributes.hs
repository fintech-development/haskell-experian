module Experian.Model.ClearIncomeAttributes where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson.Deriving (ConstructorTagModifier, DropPrefix, GenericEncoded (..), Uppercase, type (:=))
import Data.Text (Text)
import Data.Time (Day)
import Experian.Model.Common (Action, ExperianEncoding)
import GHC.Generics (Generic)

data ClearIncomeAttributes = ClearIncomeAttributes
  { action :: Action,
    denyCodes :: Maybe Text,
    denyDescriptions :: Maybe Text,
    exceptionDescriptions :: Maybe Text,
    filterCodes :: Maybe Text,
    filterDescriptions :: Maybe Text,
    highIncome1YearAgo :: Maybe Int,
    lowIncome1YearAgo :: Maybe Int,
    medianIncome90DaysAgo :: Maybe Int,
    medianIncome180DaysAgo :: Maybe Int,
    medianIncome1YearAgo :: Maybe Int,
    modeIncome90DaysAgo :: Maybe Int,
    modeIncome180DaysAgo :: Maybe Int,
    modeIncome1YearAgo :: Maybe Int,
    numberOfIncomes90DaysAgo :: Maybe Int,
    numberOfIncomes180DaysAgo :: Maybe Int,
    numberOfIncomes1YearAgo :: Maybe Int,
    numberOfDaysSinceLastIncomeReported :: Maybe Int,
    lastHousingStatus :: Maybe HousingStatus,
    numberOfDaysSinceFirstAddressMatch :: Maybe Int,
    numberOfDaysSinceLastAddressNonmatch :: Maybe Int,
    numberOfDaysSinceFirstWorkPhoneMatch :: Maybe Int,
    numberOfDaysSinceLastWorkPhoneNonmatch :: Maybe Int,
    lastPayFrequency :: Maybe Text,
    nextPaydateCalc :: Maybe Day,
    numberOfDaysSinceLastPaydateReported :: Maybe Int,
    lastDirectDeposit :: Maybe Bool,
    hit :: Maybe Bool
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ExperianEncoding ClearIncomeAttributes

data HousingStatus
  = HousingRent
  | HousingOwn
  | HousingFamily
  | HousingFriend
  | HousingOther
  deriving (Show, Eq, Generic, Bounded, Enum)
  deriving
    (ToJSON, FromJSON)
    via GenericEncoded
          '[ ConstructorTagModifier := [Uppercase, DropPrefix "Housing"]
           ]
          HousingStatus
