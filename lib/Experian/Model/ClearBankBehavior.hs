module Experian.Model.ClearBankBehavior where

import Data.Aeson (FromJSON)
import Data.Aeson.Deriving (GenericEncoded (..))
import Data.Aeson.Types (ToJSON)
import qualified Data.Text as T
import qualified Data.Time as Time
import Experian.Model.Common (Action, ExperianEncoding)
import GHC.Generics (Generic)

data ClearBankBehavior = ClearBankBehavior
  { action :: Action,
    denyDescriptions :: Maybe T.Text,
    exceptionDescriptions :: Maybe T.Text,
    filterCodes :: Maybe T.Text,
    filterDescriptions :: Maybe T.Text,
    fullName :: Maybe T.Text,
    productDate :: Maybe Time.UTCTime,
    reasonCodes :: Maybe T.Text,
    cbbReasonCodes :: Maybe T.Text,
    cbbReasonCodes2 :: Maybe T.Text,
    reasonCodeDescription :: Maybe T.Text,
    cbbReasonCodeDescription :: Maybe T.Text,
    cbbReasonCodeDescription2 :: Maybe T.Text,
    cbbNonScorableReason :: Maybe T.Text,
    cbbNonScorableReasonDescription :: Maybe T.Text,
    cbbScore :: Maybe T.Text,
    cbbScore2 :: Maybe T.Text,
    checkCashingHistory :: Maybe Bool,
    daysSinceLastCheckCashingActivity :: Maybe Int,
    daysSinceLastSuccessfulCheckCashed :: Maybe Int,
    denyCodes :: Maybe T.Text,
    estimatedBankHistory :: Maybe Int,
    maxNumberOfSsnsWithMicr :: Maybe Int,
    numberOfAccountsActive :: Maybe Int,
    numberOfAccountsAll :: Maybe Int,
    numberOfAccountsAtHighRiskBanks :: Maybe Int,
    numberOfAccountsLinkedToFraud :: Maybe Int,
    numberOfAccountsWithAlternateSsns :: Maybe Int,
    numberOfAccountsWithCheckHistory :: Maybe Int,
    numberOfAccountsWithDefaultHistory :: Maybe Int,
    numberOfHighRiskAccounts :: Maybe Int,
    numberOfLowRiskAccounts :: Maybe Int,
    numberOfUnknownRiskAccounts :: Maybe Int,
    positiveCheckWritingHistory :: Maybe Bool,
    accounts :: Maybe [Account],
    inquiryClusterAccountStability :: Stability Float,
    accountStability :: Stability Int,
    inquiryClusterStability :: Stability Int,
    fisChexAdvisor :: Maybe FisChexAdvisor,
    checkCashing :: Maybe CheckCashing
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ExperianEncoding ClearBankBehavior

data CheckCashing = CheckCashing
  { countChecksCashed :: Maybe Int,
    daysSinceLastCheckCashed :: Maybe Int,
    daysSinceLastCheckAttempted :: Maybe Int,
    micrSsn24months :: Maybe Int,
    micrSsn24monthsAttempted :: Maybe Int,
    countOfChecksCashed :: Stability Int,
    countOfChecksAttempted :: Stability Int,
    amountOfChecksCashed :: Stability Int,
    amountOfChecksAttempted :: Stability Int,
    averageAmountOfChecksCashed :: Stability Int,
    averageAmountOfChecksAttempted :: Stability Int
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ExperianEncoding CheckCashing

data FisChexAdvisor = FisChexAdvisor
  { avgNumberDaysBetweenClosure5YearsAgo :: Maybe Int,
    avgNumberOfChecksOrdered :: Maybe Int,
    consumerDisputeQuantity5YearsAgo :: Maybe Int,
    consumerDisputeResolvedQuantity5YearsAgo :: Maybe Int,
    consumerPrivacyMessageText :: Maybe T.Text,
    consumerStatementText1 :: Maybe T.Text,
    consumerStatementText2 :: Maybe T.Text,
    consumerStatementText3 :: Maybe T.Text,
    debitBureauReasonCodes :: Maybe T.Text,
    debitBureauReasonTexts :: Maybe T.Text,
    debitBureauScore :: Maybe Int,
    driverLicenseValidationMessageText :: Maybe T.Text,
    episodeSpanAllItemTotalNumberOfDays3YearsAgo :: Maybe Int,
    errorDetails :: Maybe [T.Text],
    governmentNumberValidationMessageText :: Maybe T.Text,
    maxAmountOpenItem3YearsAgo :: Maybe Int,
    maxAmountPaidItem3YearsAgo :: Maybe T.Text,
    maxNumberDaysToPay3YearsAgo :: Maybe Int,
    minCheckNumberOnOpenItem3YearsAgo :: Maybe Int,
    minNumberDaysToPay3YearsAgo :: Maybe Int,
    noClosures :: Maybe Bool,
    noDebitDataFound :: Maybe Bool,
    noPreviousInquiryDdaAndNonDda :: Maybe Bool,
    noPreviousInquiry :: Maybe Bool,
    noReturnedChecks :: Maybe Bool,
    numberDaysMostRecentOpenItem3YearsAgo :: Maybe Int,
    numberDaysMostRecentPaidItem3YearsAgo :: Maybe Int,
    numberDaysSinceFirstClosure5YearsAgo :: Maybe Int,
    numberDaysSinceMostRecentClosure5YearsAgo :: Maybe Int,
    numberOfDaysSinceFirstInquiry :: Maybe Int,
    numberOfDaysSinceFirstOrder :: Maybe Int,
    numberOfDaysSinceLastInquiry :: Maybe Int,
    numberOfDaysSinceMostRecentOrder :: Maybe Int,
    numberOfDifferentAccounts :: Maybe Int,
    numberOfInquiriesIn3YearsAgoDdaAndNonDda :: Maybe Int,
    closures :: [Closure],
    returnedChecks :: [ReturnedCheck],
    amountFraudClosures :: Stability Int,
    numberFraudClosure :: Stability Int,
    amountClosures :: Stability Int,
    amountClosuresPaid :: Stability Int,
    amountClosuresUnpaid :: Stability Int,
    numberClosures :: Stability Int,
    numberClosuresPaid :: Stability Int,
    numberClosuresUnpaid :: Stability Int,
    numberOfChecksOrdered :: Stability Int,
    numberOfCheckOrders :: Stability Int,
    numberOfNonDdaInquiries :: Stability Int,
    numberOfPaydayInquiries :: Stability Int,
    numberOfInquiries :: Stability Int,
    amountOpenItems :: Stability Int,
    amountPaidItems :: Stability Int,
    numberOpenItems :: Stability Int,
    numberPaidItems :: Stability Int
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ExperianEncoding FisChexAdvisor

data ReturnedCheck = ReturnedCheck
  { accountNumber :: Maybe T.Text,
    amount :: Maybe Int,
    checkDate :: Maybe Time.Day,
    checkNumber :: Maybe T.Text,
    driversLicenseNumber :: Maybe T.Text,
    driversLicenseState :: Maybe T.Text,
    merchantName :: Maybe T.Text,
    paidDate :: Maybe Time.Day,
    routingNumber :: Maybe T.Text,
    daysSinceCheckPaid :: Maybe Int,
    daysSinceCheckReturned :: Maybe Int,
    driversLicenseNumberMatch :: Maybe Bool,
    driversLicenseStateMatch :: Maybe Bool,
    matchesClaritySeenAccount :: Maybe Bool
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ExperianEncoding ReturnedCheck

data Closure = Closure
  { accountNumber :: Maybe T.Text,
    amount :: Maybe Int,
    closureDate :: Maybe Time.Day,
    consumerDisputeText :: Maybe T.Text,
    institutionName :: Maybe T.Text,
    institutionState :: Maybe T.Text,
    paidDate :: Maybe Time.Day,
    reasonCode :: Maybe T.Text,
    routingNumber :: Maybe T.Text,
    daysSinceClosed :: Maybe Int,
    daysSincePaidClosed :: Maybe Int,
    matchesClaritySeenAccount :: Maybe Bool
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ExperianEncoding Closure

data Stability a = Stability
  { oneMinuteAgo :: Maybe a,
    tenMinutesAgo :: Maybe a,
    oneHourAgo :: Maybe a,
    twentyfourHoursAgo :: Maybe a,
    sevenDaysAgo :: Maybe a,
    fifteenDaysAgo :: Maybe a,
    thirtyDaysAgo :: Maybe a,
    ninetyDaysAgo :: Maybe a,
    oneHundredEightyDaysAgo :: Maybe a,
    oneYearAgo :: Maybe a,
    treesixtyfiveDaysAgo :: Maybe a,
    twoYearsAgo :: Maybe a,
    threeYearsAgo :: Maybe a,
    fiveYearsAgo :: Maybe a,
    numberSinceFirstInquiry :: Maybe a,
    numberSinceLastInquiry :: Maybe a,
    name :: Maybe T.Text
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ExperianEncoding (Stability a)

data Account = Account
  { accountAgeCode :: Maybe Int,
    accountIndex :: Maybe Int,
    accountRiskLevel :: Maybe T.Text,
    bankRiskLevel :: Maybe Bool,
    daysSinceDefaultHistory :: Maybe Int,
    daysSinceFirstSeenByClarity :: Maybe Int,
    daysSinceLastSeenByClarity :: Maybe Int,
    daysSinceValidatedTrade :: Maybe Int,
    defaultHistory :: Maybe Bool,
    defaultRate60DaysAgo :: Maybe Double,
    defaultRate61365DaysAgo :: Maybe Double,
    defaultRateRatio :: Maybe Double,
    highRiskFactors :: Maybe T.Text,
    inquiries30DaysAgo :: Maybe Int,
    inquiries31365DaysAgo :: Maybe Int,
    inquiriesAppState30DaysAgo :: Maybe Int,
    inquiriesAppState31365DaysAgo :: Maybe Int,
    inquiriesAppStateRatio :: Maybe Double,
    inquiriesRatio :: Maybe Double,
    numberOfSsns :: Maybe Int,
    primary :: Maybe Bool,
    reasonCodes :: Maybe T.Text,
    validatedThroughTrades :: Maybe Bool,
    bankName :: Maybe T.Text,
    accountNumber :: Maybe T.Text,
    routingNumber :: Maybe T.Text,
    stability :: Maybe (Stability Int)
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ExperianEncoding Account
