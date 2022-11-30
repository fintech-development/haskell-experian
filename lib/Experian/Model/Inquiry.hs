module Experian.Model.Inquiry where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson.Deriving
  ( ConstructorTagModifier,
    DropPrefix,
    GenericEncoded (..),
    Uppercase,
    type (:=),
  )
import qualified Data.Text as T
import qualified Data.Time as Time
import Experian.Model.Common (Action, ExperianEncoding)
import GHC.Generics (Generic)

data Inquiry = Inquiry
  { accountAge :: Maybe Int,
    action :: Action,
    denyCodes :: Maybe T.Text,
    denyDescriptions :: Maybe T.Text,
    exceptionDescriptions :: Maybe T.Text,
    filterCodes :: Maybe T.Text,
    filterDescriptions :: Maybe T.Text,
    activeMilitary :: Maybe Bool,
    bankAccountType :: Maybe AccountType,
    bankRoutingNumber :: Maybe T.Text,
    bankRoutingValid :: Maybe Bool,
    cellPhone :: Maybe T.Text,
    city :: Maybe T.Text,
    controlFileName :: T.Text,
    controlFileVersionNumber :: Maybe Int,
    dateOfBirth :: Maybe Time.Day,
    dateOfLastActivity :: Maybe Time.UTCTime,
    dateOfNextPayday :: Maybe Time.Day,
    debitCardNumber :: Maybe T.Text,
    debitCardExpiration :: Maybe Time.Day,
    driversLicenseInvalid :: Maybe Bool,
    driversLicenseNumber :: Maybe T.Text,
    driversLicenseState :: Maybe T.Text,
    emailAddress :: Maybe T.Text,
    emailDomainName :: Maybe T.Text,
    emailName :: Maybe T.Text,
    employerAddress :: Maybe T.Text,
    employerCity :: Maybe T.Text,
    employerName :: Maybe T.Text,
    employerState :: Maybe T.Text,
    firstName :: T.Text,
    generationalCode :: Maybe Generation,
    homePhone :: Maybe T.Text,
    housingStatus :: Maybe HousingStatus,
    initiatingInquiry :: Maybe T.Text,
    inquiryPurposeType :: InquiryPurposeType,
    inquiryReceivedAt :: Maybe Time.UTCTime,
    inquiryTradelineType :: Maybe T.Text,
    lastName :: Maybe T.Text,
    lastPurchased :: Maybe Time.UTCTime,
    lastPurchasedByGroup :: Maybe Time.UTCTime,
    lastSeenByAccount :: Maybe Time.UTCTime,
    lastSeenByGroup :: Maybe Time.UTCTime,
    lastSeenByLocation :: Maybe Time.UTCTime,
    loanDuration :: Maybe Int,
    middleInitial :: Maybe T.Text,
    monthsAtAddress :: Maybe Int,
    monthsAtCurrentEmployer :: Maybe Int,
    netMonthlyIncome :: Maybe Int,
    numberOfSsnsWithBankAccount :: Maybe Int,
    occupationType :: Maybe T.Text,
    ofacMatch :: Maybe Bool,
    ofacScore :: Maybe Int,
    originationIpAddress :: Maybe T.Text,
    passThrough1 :: Maybe T.Text,
    passThrough2 :: Maybe T.Text,
    passThrough3 :: Maybe T.Text,
    passThrough4 :: Maybe T.Text,
    passThrough5 :: Maybe T.Text,
    payFrequency :: Maybe PayFrequency,
    paycheckDirectDeposit :: Maybe Bool,
    pcDigitalFingerprint :: Maybe T.Text,
    pcFingerprintTimeOffset :: Maybe T.Text,
    pcpsession :: Maybe T.Text,
    pcpvendor :: Maybe T.Text,
    fullName :: Maybe T.Text,
    productDate :: Maybe Time.UTCTime,
    productsExecuted :: Maybe T.Text,
    productsRequested :: Maybe T.Text,
    referenceFirstName :: Maybe T.Text,
    referenceLastName :: Maybe T.Text,
    referencePhone :: Maybe T.Text,
    referenceRelationship :: Maybe Relationship,
    requestedAmount :: Maybe Int,
    deliveryPoint :: Maybe T.Text,
    leadgen :: Maybe T.Text,
    server :: Maybe T.Text,
    socialSecurityDeceased :: Maybe Bool,
    socialSecurityValid :: Maybe Bool,
    state :: Maybe T.Text,
    streetAddress1 :: Maybe T.Text,
    streetAddress2 :: Maybe T.Text,
    trackingNumber :: Maybe T.Text,
    username :: Maybe T.Text,
    workFaxNumber :: Maybe T.Text,
    workPhone :: Maybe T.Text,
    workPhoneExtension :: Maybe T.Text,
    zipCode :: Maybe T.Text,
    socialSecurityNumber :: Maybe T.Text,
    bankAccountNumber :: Maybe T.Text,
    bankAccountZeros :: Maybe Int,
    totalHistoricalInquiries :: Maybe Int,
    ssnFirstNameCount :: Maybe Int,
    ssnLastNameCount :: Maybe Int,
    ssnFirstLastNameCount :: Maybe Int,
    ssnDistinctFirstLastNameCount :: Maybe Int,
    ssnFirstAppearance :: Maybe Bool,
    vin :: Maybe T.Text
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ExperianEncoding Inquiry

data AccountType
  = AccountChecking
  | AccountSavings
  | AccountDebitCard
  | AccountMoneyMarket
  | AccountIIN
  | AccountOther
  | AccountDepository
  deriving (Show, Eq, Generic, Bounded, Enum)
  deriving
    (ToJSON, FromJSON)
    via GenericEncoded
          '[ConstructorTagModifier := '[Uppercase, DropPrefix "Account"]]
          AccountType

data Generation
  = GenerationJR
  | GenerationSR
  | Generation2
  | Generation3
  | Generation4
  | Generation5
  | Generation6
  | Generation7
  | Generation8
  | Generation9
  deriving (Show, Eq, Generic, Bounded, Enum)
  deriving (ToJSON, FromJSON) via GenericEncoded '[ConstructorTagModifier := DropPrefix "Generation"] Generation

data InquiryPurposeType
  = AR
  | AS
  | RA
  | RP
  | CL
  | PC
  | MS
  | CC
  | CS
  | PS
  | IV
  | IS
  | EH
  | ES
  | LH
  | LS
  | WS
  | WH
  | PR
  | PA
  | SP
  | CA
  deriving (Show, Eq, Generic, Bounded, Enum)
  deriving (ToJSON, FromJSON) via ExperianEncoding InquiryPurposeType

data PayFrequency
  = Daily
  | Weekly
  | Biweekly
  | Semimonthly
  | Monthly
  deriving (Show, Eq, Generic, Bounded, Enum)
  deriving (ToJSON, FromJSON) via GenericEncoded '[ConstructorTagModifier := Uppercase] PayFrequency

data Relationship
  = Relative
  | Friend
  | Employer
  | Neighbor
  deriving (Show, Eq, Generic, Bounded, Enum)
  deriving (ToJSON, FromJSON) via GenericEncoded '[ConstructorTagModifier := Uppercase] Relationship

data HousingStatus
  = HousingRent
  | HousingOwn
  | HousingFamily
  | HousingFriend
  | HousingOther
  deriving (Show, Eq, Generic, Bounded, Enum)
  deriving (ToJSON, FromJSON) via GenericEncoded '[ConstructorTagModifier := '[Uppercase, DropPrefix "Housing"]] HousingStatus
