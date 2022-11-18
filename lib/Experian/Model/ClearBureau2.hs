module Experian.Model.ClearBureau2 where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson.Deriving (FieldLabelModifier, GenericEncoded (..), type (:=))
import Data.Text (Text)
import Data.Time (UTCTime)
import Experian.Model.Common (Action, ExperianEncoding, Replace)
import GHC.Generics (Generic)

type ClearBureau2Encoding =
  GenericEncoded
    '[ FieldLabelModifier
         := '[Replace "_type" "type"]
     ]

data ClearBureau2 = ClearBureau2
  { action :: Action,
    denyCodes :: Maybe Text,
    denyDescriptions :: Maybe Text,
    exceptionDescriptions :: Maybe Text,
    filterCodes :: Maybe Text,
    filterDescriptions :: Maybe Text,
    productDate :: Maybe UTCTime,
    reasonCodeDescription :: Maybe Text,
    reasonCodes :: Maybe Text,
    score :: Maybe Int,
    ccpReport :: Maybe CcpReport
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ExperianEncoding ClearBureau2

data CcpReport = CcpReport
  { addressInformation :: Maybe [AddressInformation],
    consumerIdentity :: Maybe ConsumerIdentity,
    employmentInformation :: Maybe [EmploymentInformation],
    fraudShield :: Maybe [FraudShield],
    informationalMessage :: Maybe [InformationalMessage],
    inquiry :: Maybe [ClearBureau2Inquiry],
    summaries :: Maybe [Summary],
    publicRecord :: Maybe [PublicRecord],
    riskModel :: Maybe [RiskModel],
    ssn :: Maybe [Ssn],
    statement :: Maybe [Statement],
    tradeline :: Maybe [Tradeline]
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ClearBureau2Encoding CcpReport

data AddressInformation = AddressInformation
  { city :: Maybe Text,
    countyCode :: Maybe Text,
    dwellingType :: Maybe Text,
    firstReportedDate :: Maybe Text,
    lastReportingSubscriberCode :: Maybe Text,
    lastUpdatedDate :: Maybe Text,
    source :: Maybe Text,
    state :: Maybe Text,
    stateCode :: Maybe Text,
    streetName :: Maybe Text,
    streetPrefix :: Maybe Text,
    streetSuffix :: Maybe Text,
    timesReported :: Maybe Text,
    unitId :: Maybe Text,
    unitType :: Maybe Text,
    zipCode :: Maybe Text
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ClearBureau2Encoding AddressInformation

data ConsumerIdentity = ConsumerIdentity
  { dob :: Maybe DateOfBirth,
    name :: Maybe [Name],
    phone :: Maybe [Phone]
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ClearBureau2Encoding ConsumerIdentity

data DateOfBirth = DateOfBirth
  { day :: Maybe Text,
    month :: Maybe Text,
    year :: Maybe Text
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ClearBureau2Encoding DateOfBirth

data Name = Name
  { firstName :: Maybe Text,
    generationCode :: Maybe Text,
    middleName :: Maybe Text,
    secondSurname :: Maybe Text,
    surname :: Maybe Text,
    _type :: Maybe Text
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ClearBureau2Encoding Name

data Phone = Phone
  { number :: Maybe Text,
    source :: Maybe Text,
    _type :: Maybe Text
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ClearBureau2Encoding Phone

data EmploymentInformation = EmploymentInformation
  { addressExtraLine :: Maybe Text,
    addressFirstLine :: Maybe Text,
    addressSecondLine :: Maybe Text,
    firstReportedDate :: Maybe Text,
    lastUpdatedDate :: Maybe Text,
    name :: Maybe Text,
    source :: Maybe Text,
    zipCode :: Maybe Text
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ClearBureau2Encoding EmploymentInformation

data FraudShield = FraudShield
  { addressCount :: Maybe Text,
    addressDate :: Maybe Text,
    addressErrorCode :: Maybe Text,
    dateOfBirth :: Maybe Text,
    dateOfDeath :: Maybe Text,
    sic :: Maybe Text,
    socialCount :: Maybe Text,
    socialDate :: Maybe Text,
    socialErrorCode :: Maybe Text,
    ssnFirstPossibleIssuanceYear :: Maybe Text,
    ssnLastPossibleIssuanceYear :: Maybe Text,
    text :: Maybe Text,
    _type :: Maybe Text,
    fraudShieldIndicators :: Maybe FraudShieldIndicators
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ClearBureau2Encoding FraudShield

newtype FraudShieldIndicators = FraudShieldIndicators
  { indicator :: Maybe [Text]
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ClearBureau2Encoding FraudShieldIndicators

data InformationalMessage = InformationalMessage
  { messageNumber :: Maybe Text,
    messageNumberDetailed :: Maybe Text,
    messageText :: Maybe Text
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ClearBureau2Encoding InformationalMessage

data ClearBureau2Inquiry = ClearBureau2Inquiry
  { amount :: Maybe Text,
    date :: Maybe Text,
    kob :: Maybe Text,
    subscriberCode :: Maybe Text,
    subscriberName :: Maybe Text,
    terms :: Maybe Text,
    _type :: Maybe Text
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ClearBureau2Encoding ClearBureau2Inquiry

data Summary = Summary
  { summaryType :: Maybe Text,
    attributes :: [Attribute]
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ClearBureau2Encoding Summary

data Attribute = Attribute
  { id :: Maybe Text,
    value :: Maybe Text
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ClearBureau2Encoding Attribute

data PublicRecord = PublicRecord
  { adjustmentPercent :: Maybe Text,
    amount :: Maybe Text,
    bankruptcyAssetAmount :: Maybe Text,
    bankruptcyVoluntaryIndicator :: Maybe Text,
    bookPageSequence :: Maybe Text,
    consumerComment :: Maybe Text,
    courtCode :: Maybe Text,
    courtName :: Maybe Text,
    disputeFlag :: Maybe Text,
    ecoa :: Maybe Text,
    evaluation :: Maybe Text,
    filingDate :: Maybe Text,
    plaintiffName :: Maybe Text,
    referenceNumber :: Maybe Text,
    repaymentPercent :: Maybe Text,
    status :: Maybe Text,
    statusDate :: Maybe Text
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ClearBureau2Encoding PublicRecord

data RiskModel = RiskModel
  { evaluation :: Maybe Text,
    modelIndicator :: Maybe Text,
    score :: Maybe Text,
    scoreFactors :: Maybe [ScoreFactor]
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ClearBureau2Encoding RiskModel

data ScoreFactor = ScoreFactor
  { importance :: Maybe Text,
    code :: Maybe Text
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ClearBureau2Encoding ScoreFactor

data Ssn = Ssn
  { number :: Maybe Text,
    ssnIndicators :: Maybe Text,
    variationIndicator :: Maybe Text
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ClearBureau2Encoding Ssn

data Statement = Statement
  { dateReported :: Maybe Text,
    statementText :: Maybe Text,
    _type :: Maybe Text
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ClearBureau2Encoding Statement

data Tradeline = Tradeline
  { accountNumber :: Maybe Text,
    accountType :: Maybe Text,
    amount1 :: Maybe Text,
    amount1Qualifier :: Maybe Text,
    amount2 :: Maybe Text,
    amount2Qualifier :: Maybe Text,
    amountBalloonPayment :: Maybe Text,
    amountPastDue :: Maybe Text,
    balanceAmount :: Maybe Text,
    balanceDate :: Maybe Text,
    bankruptcyChapterNumber :: Maybe Text,
    consumerComment :: Maybe Text,
    consumerDisputeFlag :: Maybe Text,
    datePaymentDue :: Maybe Text,
    delinquencies30Days :: Maybe Text,
    delinquencies60Days :: Maybe Text,
    delinquencies90to180Days :: Maybe Text,
    derogCounter :: Maybe Text,
    ecoa :: Maybe Text,
    evaluation :: Maybe Text,
    kob :: Maybe Text,
    lastPaymentDate :: Maybe Text,
    maxDelinquencyDate :: Maybe Text,
    monthlyPaymentAmount :: Maybe Text,
    monthlyPaymentType :: Maybe Text,
    monthsHistory :: Maybe Text,
    openDate :: Maybe Text,
    openOrClosed :: Maybe Text,
    originalCreditorName :: Maybe Text,
    paymentHistory :: Maybe Text,
    revolvingOrInstallment :: Maybe Text,
    soldToName :: Maybe Text,
    specialComment :: Maybe Text,
    status :: Maybe Text,
    statusDate :: Maybe Text,
    subscriberCode :: Maybe Text,
    subscriberName :: Maybe Text,
    terms :: Maybe Text,
    enhancedPaymentData :: Maybe EnhancedPaymentData
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ClearBureau2Encoding Tradeline

data EnhancedPaymentData = EnhancedPaymentData
  { actualPaymentAmount :: Maybe Text,
    chargeoffAmount :: Maybe Text,
    ciiCode :: Maybe Text,
    complianceCondition :: Maybe Text,
    creditLimitAmount :: Maybe Text,
    enhancedAccountCondition :: Maybe Text,
    enhancedAccountType :: Maybe Text,
    enhancedPaymentHistory84 :: Maybe Text,
    enhancedPaymentStatus :: Maybe Text,
    enhancedSpecialComment :: Maybe Text,
    enhancedTerms :: Maybe Text,
    enhancedTermsFrequency :: Maybe Text,
    firstDelinquencyDate :: Maybe Text,
    highBalanceAmount :: Maybe Text,
    maxDelinquencyCode :: Maybe Text,
    mortgageId :: Maybe Text,
    originalCreditorClassificationCode :: Maybe Text,
    originalLoanAmount :: Maybe Text,
    paymentLevelDate :: Maybe Text,
    purchasedPortfolioFromName :: Maybe Text,
    secondDelinquencyDate :: Maybe Text,
    secondaryAgencyCode :: Maybe Text,
    secondaryAgencyId :: Maybe Text,
    specialPaymentAmount :: Maybe Text,
    specialPaymentCode :: Maybe Text,
    specialPaymentDate :: Maybe Text
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ClearBureau2Encoding EnhancedPaymentData
