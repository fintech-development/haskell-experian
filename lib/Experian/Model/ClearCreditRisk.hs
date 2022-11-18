module Experian.Model.ClearCreditRisk where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson.Deriving (GenericEncoded (..))
import Data.Text (Text)
import Data.Time (UTCTime)
import Experian.Model.ClearBankBehavior (Stability)
import Experian.Model.Common (Action, ExperianEncoding)
import GHC.Generics (Generic)

data ClearCreditRisk = ClearCreditRisk
  { action :: Action,
    denyCodes :: Maybe Text,
    denyDescriptions :: Maybe Text,
    exceptionDescriptions :: Maybe Text,
    filterCodes :: Maybe Text,
    filterDescriptions :: Maybe Text,
    activeDutyErrorMessage :: Maybe Text,
    activeDutyStatus :: Maybe Int,
    amountOfLoans :: Maybe Int,
    amountOfLoansCurrentAndOpen :: Maybe Int,
    amountOfLoansPaidOff :: Maybe Int,
    amountOfLoansPastDue :: Maybe Int,
    claritySeen :: Maybe Bool,
    currentInquiryClusterPosition :: Maybe Int,
    daysSinceFirstBankAccountFirstSeen :: Maybe Int,
    daysSinceFirstBankAccountPreviouslySeen :: Maybe Int,
    daysSinceFirstLoanOpened :: Maybe Int,
    daysSinceFirstLoanPaidOff :: Maybe Int,
    daysSinceFirstOntimePayment :: Maybe Int,
    daysSinceInquiryFirstSeen :: Maybe Int,
    daysSinceInquiryPreviouslySeen :: Maybe Int,
    daysSinceLastLoanChargedOff :: Maybe Int,
    daysSinceLastLoanInCollections :: Maybe Int,
    daysSinceLastLoanOpened :: Maybe Int,
    daysSinceLastLoanPaidOff :: Maybe Int,
    daysSinceLastLoanPayment :: Maybe Int,
    daysSinceLastOntimePayment :: Maybe Int,
    daysSincePreviousBankAccountFirstSeen :: Maybe Int,
    daysSincePreviousBankAccountPreviouslySeen :: Maybe Int,
    daysSinceReportedIncomePreviouslySeen :: Maybe Int,
    fullName :: Maybe Text,
    highestNumberOfDaysPastDue :: Maybe Int,
    hit :: Maybe Bool,
    inquirySeenExcluding484d :: Maybe Bool,
    inquirySeenIncluding48 :: Maybe Bool,
    messages :: Maybe Text,
    nonScorableReasonCode :: Maybe Text,
    nonScorableReasonDescription :: Maybe Text,
    numberOfBankAccounts :: Maybe Int,
    numberOfEmployersLastSixMonths :: Maybe Int,
    numberOfLoans :: Maybe Int,
    numberOfLoansCurrentAndOpen :: Maybe Int,
    numberOfLoansPaidOff :: Maybe Int,
    numberOfLoansPastDue :: Maybe Int,
    productDate :: Maybe UTCTime,
    reasonCodeDescription :: Maybe Text,
    reasonCodes :: Maybe Text,
    reportedNetMonthlyIncomePreviouslySeen :: Maybe Int,
    score :: Maybe Int,
    tooManyInquiries :: Maybe Bool,
    tooManyTradelines :: Maybe Bool,
    worstPaymentRating :: Maybe Text,
    experianAttribute :: Maybe ExperianAttribute,
    stabilities :: [Stability Int],
    inquiries :: Maybe [ClearCreditRiskInquiry],
    tradelines :: Maybe [Tradeline]
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ExperianEncoding ClearCreditRisk

data ExperianAttribute = ExperianAttribute
  { all7360 :: Maybe Int,
    all7936 :: Maybe Int,
    all8221 :: Maybe Int,
    all9120 :: Maybe Int,
    all9121 :: Maybe Int,
    all9122 :: Maybe Int,
    all9123 :: Maybe Int,
    all9124 :: Maybe Int,
    all9125 :: Maybe Int,
    all9128 :: Maybe Int,
    als2000 :: Maybe Int,
    aua6280 :: Maybe Int,
    bcc2688 :: Maybe Int,
    bcc6160 :: Maybe Int,
    bcc7216 :: Maybe Int,
    bcx7110 :: Maybe Int,
    brc5747 :: Maybe Int,
    brc7140 :: Maybe Int,
    brc7160 :: Maybe Int,
    col3210 :: Maybe Int,
    fip1380 :: Maybe Int,
    fip6200 :: Maybe Int,
    iln6230 :: Maybe Int,
    iln7150 :: Maybe Int,
    rev0300 :: Maybe Int,
    rev4080 :: Maybe Int,
    rev7432 :: Maybe Int,
    rev8151 :: Maybe Int,
    rtr5038 :: Maybe Int,
    tbca0455 :: Maybe Int,
    tbca2624 :: Maybe Int,
    tbca3283 :: Maybe Int,
    tbcc2303 :: Maybe Int,
    tbcc2351 :: Maybe Int,
    tbcc3203 :: Maybe Int,
    tcol3553 :: Maybe Int,
    tpil2712 :: Maybe Int,
    trtr3752 :: Maybe Int,
    trtr4280 :: Maybe Int
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ExperianEncoding ExperianAttribute

data ClearCreditRiskInquiry = ClearCreditRiskInquiry
  { inquiryReceivedAt :: Maybe UTCTime,
    inquiryPurposeType :: Maybe Text,
    inquiryTradelineType :: Maybe Text,
    memberId :: Maybe Text
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ExperianEncoding ClearCreditRiskInquiry

data Tradeline = Tradeline
  { accountOpened :: Maybe UTCTime,
    accountNumber :: Maybe Text,
    amountPastDue :: Maybe Int,
    closedDate :: Maybe UTCTime,
    consumerAccountNumber :: Maybe Int,
    countOneCyclesPastDue :: Maybe Int,
    countThreeCyclesPastDue :: Maybe Int,
    countTwoCyclesPastDue :: Maybe Int,
    creditLimit :: Maybe Int,
    currentBalance :: Maybe Int,
    cyclesSummarized :: Maybe Int,
    delinquencyDate :: Maybe UTCTime,
    ecoaCode :: Maybe Text,
    highestCredit :: Maybe Int,
    lastUpdatedAt :: Maybe UTCTime,
    memberId :: Maybe Text,
    mop :: Maybe Text,
    paymentHistory :: Maybe Text,
    specialComment :: Maybe Text,
    terms :: Maybe Text,
    tradelineType :: Maybe Text
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ExperianEncoding Tradeline
