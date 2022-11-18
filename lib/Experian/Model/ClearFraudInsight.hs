module Experian.Model.ClearFraudInsight where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson.Deriving (GenericEncoded (..))
import Data.Text (Text)
import Experian.Model.ClearBankBehavior (Stability)
import Experian.Model.Common (Action, ExperianEncoding)
import GHC.Generics (Generic)

data ClearFraudInsight = ClearFraudInsight
  { action :: Action,
    denyCodes :: Maybe Text,
    denyDescriptions :: Maybe Text,
    exceptionDescriptions :: Maybe Text,
    filterCodes :: Maybe Text,
    filterDescriptions :: Maybe Text,
    crosstabMultiple :: Maybe Float,
    crosstabPointsTotal :: Maybe Int,
    errorCode :: Maybe Text,
    errorDescription :: Maybe Text,
    fraudSignatureIdentifier :: Maybe Text,
    fraudSignatureMatch :: Maybe Bool,
    fraudSignatureMatchCount :: Maybe Int,
    fraudSignatureName :: Maybe Text,
    hit :: Maybe Bool,
    message :: Maybe Text,
    nonScorableReasonCode :: Maybe Text,
    nonScorableReasonDescription :: Maybe Text,
    reasonCodeDescription :: Maybe Text,
    reasonCodes :: Maybe Text,
    scoreVersion :: Maybe Int,
    score :: Maybe Int,
    stabilityNonScorableReasonCode :: Maybe Text,
    stabilityNonScorableReasonDescription :: Maybe Text,
    stabilityReasonCodeDescription :: Maybe Text,
    stabilityReasonCodes :: Maybe Text,
    stabilityScore :: Maybe Int,
    indicator :: Maybe Indicator,
    identityVerification :: Maybe IdentityVerification,
    pointsTotal :: Maybe (Stability Int),
    multiple :: Maybe (Stability Float),
    stabilities :: [Stability Int],
    crosstabs :: [Crosstab],
    ratios :: [Stability Float]
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ExperianEncoding ClearFraudInsight

data Indicator = Indicator
  { bankType :: Maybe Text,
    bestOnFileSsnIssueDateCannotBeVerified :: Maybe Bool,
    bestOnFileSsnRecordedAsDeceased :: Maybe Bool,
    creditEstablishedBeforeAge18 :: Maybe Bool,
    creditEstablishedPriorToSsnIssueDate :: Maybe Bool,
    crossAbaNumberOfSsnsWithBankAccount :: Maybe Int,
    crossAbaUniqueAbaCount :: Maybe Int,
    currentAddressReportedByNewTradeOnly :: Maybe Bool,
    currentAddressReportedByTradeOpenLt90Days :: Maybe Bool,
    driversLicenseFormatInvalid :: Maybe Bool,
    highProbabilitySsnBelongsToAnother :: Maybe Bool,
    inStateBankIndicator :: Maybe Bool,
    inputSsnInvalid :: Maybe Bool,
    inputSsnIssueDateCannotBeVerified :: Maybe Bool,
    inputSsnRecordedAsDeceased :: Maybe Bool,
    inquiryAddressCautious :: Maybe Bool,
    inquiryAddressFirstReportedLt90Days :: Maybe Bool,
    inquiryAddressHighRisk :: Maybe Bool,
    inquiryAddressNonResidential :: Maybe Bool,
    inquiryAgeYoungerThanSsnIssueDate :: Maybe Bool,
    inquiryCurrentAddressNotOnFile :: Maybe Bool,
    inquiryOnFileCurrentAddressConflict :: Maybe Bool,
    maxNumberOfSsnsWithAnyBankAccount :: Maybe Int,
    moreThan3InquiriesInTheLast30Days :: Maybe Bool,
    nearestBankBranchDistance :: Maybe Int,
    onFileAddressCautious :: Maybe Bool,
    onFileAddressHighRisk :: Maybe Bool,
    onFileAddressNonResidential :: Maybe Bool,
    ssnReportedMoreFrequentlyForAnother :: Maybe Bool,
    telephoneNumberInconsistentWithAddress :: Maybe Bool,
    totalNumberOfFraudIndicators :: Maybe Int,
    workPhonePreviouslyListedAsCellPhone :: Maybe Bool,
    workPhonePreviouslyListedAsHomePhone :: Maybe Bool
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ExperianEncoding Indicator

data IdentityVerification = IdentityVerification
  { dobMatchCode :: Maybe Text,
    dobMatchDescription :: Maybe Text,
    dobMatchResult :: Maybe Text,
    driversLicenseMatchCode :: Maybe Text,
    driversLicenseMatchDescription :: Maybe Text,
    driversLicenseMatchResult :: Maybe Text,
    nameAddressMatchCode :: Maybe Text,
    nameAddressMatchConfidence :: Maybe Int,
    nameAddressMatchDescription :: Maybe Text,
    phoneNameAddressMatchCode :: Maybe Text,
    phoneNameAddressMatchConfidence :: Maybe Int,
    phoneNameAddressMatchDescription :: Maybe Text,
    ssnNameAddressMatchCode :: Maybe Text,
    ssnNameAddressMatchConfidence :: Maybe Int,
    ssnNameAddressMatchDescription :: Maybe Text,
    phoneTypeCode :: Maybe Text,
    phoneTypeDescription :: Maybe Text
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ExperianEncoding IdentityVerification

data Crosstab = Crosstab
  { name :: Maybe Text,
    ssn :: Maybe Int,
    driversLicense :: Maybe Int,
    bankAccount :: Maybe Int,
    homeAddress :: Maybe Int,
    zipCode :: Maybe Int,
    homePhone :: Maybe Int,
    cellPhone :: Maybe Int,
    emailAddress :: Maybe Int
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ExperianEncoding Crosstab
