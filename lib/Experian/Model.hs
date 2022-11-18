module Experian.Model where

import Data.Aeson
  ( FromJSON (..),
    Options (..),
    ToJSON (..),
    camelTo2,
    defaultOptions,
    genericParseJSON,
    withObject,
    (.:),
  )
import Data.Aeson.Deriving (GenericEncoded (..))
import qualified Data.Text as T
import qualified Data.Time as Time
import Experian.Model.ClearAdvancedAttributes (ClearAdvancedAttributes)
import Experian.Model.ClearBankBehavior (ClearBankBehavior)
import Experian.Model.ClearBureau2 (ClearBureau2)
import Experian.Model.ClearCreditRisk (ClearCreditRisk)
import Experian.Model.ClearFraudInsight (ClearFraudInsight)
import Experian.Model.ClearIncomeAttributes (ClearIncomeAttributes)
import Experian.Model.Common (ExperianEncoding)
import Experian.Model.Inquiry (Inquiry)
import GHC.Generics (Generic)

data InquiryRequest = InquiryRequest
  { groupId :: T.Text,
    accountId :: T.Text,
    locationId :: T.Text,
    username :: T.Text,
    password :: T.Text,
    controlFileName :: T.Text,
    bankAccountNumber :: Maybe T.Text,
    bankAccountType :: Maybe T.Text,
    bankRoutingNumber :: Maybe T.Text,
    cellPhone :: Maybe T.Text,
    city :: Maybe T.Text,
    dateOfBirth :: Maybe Time.Day,
    dateOfNextPayday :: Maybe Time.Day,
    driversLicenseNumber :: Maybe T.Text,
    driversLicenseState :: Maybe T.Text,
    emailAddress :: Maybe T.Text,
    employerAddress :: Maybe T.Text,
    employerCity :: Maybe T.Text,
    employerState :: Maybe T.Text,
    firstName :: T.Text,
    lastName :: T.Text,
    generationalCode :: Maybe T.Text,
    homePhone :: Maybe T.Text,
    housingStatus :: Maybe T.Text,
    inquiryPurposeType :: Maybe T.Text,
    inquiryTradelineType :: Maybe T.Text,
    netMonthlyIncome :: Maybe T.Text,
    payFrequency :: Maybe T.Text,
    paycheckDirectDeposit :: Maybe T.Text,
    referenceFirstName :: Maybe T.Text,
    referenceLastName :: Maybe T.Text,
    referencePhone :: Maybe T.Text,
    referenceRelationship :: Maybe T.Text,
    socialSecurityNumber :: T.Text,
    state :: Maybe T.Text,
    streetAddress_1 :: T.Text,
    streetAddress_2 :: Maybe T.Text,
    monthsAtAddress :: Maybe T.Text,
    monthsAtCurrentEmployer :: Maybe T.Text,
    workFaxNumber :: Maybe T.Text,
    workPhone :: Maybe T.Text,
    zipCode :: T.Text,
    ignoreBeforeThisDate :: Maybe Time.Day
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ExperianEncoding InquiryRequest

data InquiryResponse = InquiryResponse
  { trackingNumber :: T.Text,
    action :: T.Text,
    filterCodes :: Maybe T.Text,
    filterDescriptions :: Maybe T.Text,
    exceptionDescriptions :: Maybe T.Text,
    identityTheftPrevention :: Maybe T.Text,
    productNotices :: Maybe [ProductNotice],
    consumerAlerts :: Maybe ConsumerAlerts,
    clearProductsRequest :: Maybe ClearProductRequest,
    inquiry :: Inquiry,
    clearAdvancedAttributes :: Maybe ClearAdvancedAttributes,
    clearBankBehavior :: Maybe ClearBankBehavior,
    clearBureau2 :: Maybe ClearBureau2,
    clearCreditRisk :: Maybe ClearCreditRisk,
    clearFraudInsight :: Maybe ClearFraudInsight,
    clearIncomeAttributes :: Maybe ClearIncomeAttributes
  }
  deriving (Show, Eq, Generic)

instance FromJSON InquiryResponse where
  parseJSON = withObject "InquiryResponse" $ \obj -> do
    resp <- obj .: "xml_response"
    genericParseJSON (defaultOptions {fieldLabelModifier = camelTo2 '_'}) resp

newtype ProductNotice = ProductNotice
  { message :: T.Text
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ExperianEncoding ProductNotice

data ConsumerAlerts = ConsumerAlerts
  { factaAlerts :: FactaAlerts,
    nonFactaAlerts :: NonFactaAlerts
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ExperianEncoding ConsumerAlerts

data FactaAlerts = FactaAlerts
  { experianInitialAlerts :: Maybe [T.Text],
    experianExtendedAlerts :: Maybe [T.Text],
    experianCreditReportingAgencyAlerts :: Maybe [T.Text],
    experianActiveDutyAlerts :: Maybe [T.Text],
    chexadvisorInitialOrExtendedAlerts :: Maybe [T.Text],
    clarityInitialAlerts :: Maybe [T.Text],
    clarityExtendedAlerts :: Maybe [T.Text],
    clarityActiveDutyAlerts :: Maybe [T.Text]
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ExperianEncoding FactaAlerts

data NonFactaAlerts = NonFactaAlerts
  { experianGeneralStatements :: Maybe [T.Text],
    experianDisputeStatements :: Maybe [T.Text],
    clarityFileFreezes :: Maybe [T.Text],
    clarityDisputeStatements :: Maybe [T.Text],
    chexadvisorFileFreezes :: Maybe [T.Text],
    chexadvisorDisputeStatements :: Maybe [T.Text]
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ExperianEncoding NonFactaAlerts

data ClearProductRequest = ClearProductRequest
  { action :: T.Text,
    controlFileName :: T.Text,
    controlFileSubstituted :: Maybe T.Text,
    controlFileVersionNumber :: Maybe Int,
    filterCodes :: Maybe T.Text,
    filterDescriptions :: Maybe T.Text,
    groupName :: T.Text,
    accountName :: T.Text,
    locationName :: T.Text,
    username :: T.Text
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ExperianEncoding ClearProductRequest
