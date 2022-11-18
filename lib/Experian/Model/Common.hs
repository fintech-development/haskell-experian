{-# LANGUAGE DeriveAnyClass #-}

module Experian.Model.Common where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson.Deriving
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import GHC.TypeLits

data Action
  = Approve
  | Deny
  | Exception
  | Review
  | PositiveApprove
  deriving (Show, Eq, Generic, Bounded, Enum)
  deriving anyclass (ToJSON, FromJSON)

type ExperianEncoding = SnakeCaseFields

type SnakeCaseFields =
  GenericEncoded '[FieldLabelModifier := SnakeCase]

data Replace (a :: Symbol) (b :: Symbol)

instance (KnownSymbol s1, KnownSymbol s2) => StringFunction (Replace s1 s2) where
  stringFunction _ s = if s == symbolVal (Proxy @s1) then symbolVal (Proxy @s2) else s
