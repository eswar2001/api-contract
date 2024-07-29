{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE StandaloneDeriving #-}

module ApiContract.Types where

import Data.Aeson (ToJSON,FromJSON)
import GHC.Generics (Generic)
import qualified Data.Map as Map
import GHC.Hs (SrcSpanAnnA)
import GHC (SrcSpan(..), RealSrcSpan(..))

data TypeOfInstance =
      Derived
      | Custom
   deriving (Eq,Show, Generic,FromJSON,ToJSON)

data CaseType =
       SnakeCase
       | CamelCase
       | PascalCase
       | KebabCase
   deriving (Eq,Show, Generic,FromJSON,ToJSON)

data InstancePresence =
       ToJSON
       | ParseJSON
       | ToEncoding
   deriving (Eq,Show, Generic,FromJSON,ToJSON)

data TypeRule = TypeRule
   { caseType  :: Maybe CaseType
   , dataConstructors :: Map.Map String DataConInfo
   , instances :: Map.Map String InstanceFromTC
   , typeKind :: String
   } deriving (Show, Generic,FromJSON,ToJSON)

data Types = Types
   { types :: Map.Map String TypeRule
   } deriving (Show, Generic,FromJSON,ToJSON)

data DataConInfo = DataConInfo
  { fields'      :: Map.Map String String
  , sumTypes    :: [String]
  } deriving (Show, Eq, Ord,Generic,ToJSON,FromJSON)

data InstanceFromTC = InstanceFromTC
   {
      fieldsList :: [String]
      , typeOfInstance :: TypeOfInstance
   }
   deriving (Show, Generic,FromJSON,ToJSON)

data ApiContractError =
    -- fieldName typeName
    MISSING_FIELD String String
    -- fieldName expectedType typeFromCode typeName
    | TYPE_MISMATCH String String String String
    -- dataConName typeName
    | MISSING_DATACON String String
    -- typeName fieldName caseType
    | FIELD_CASE_MISMATCH String String CaseType
    -- typeName
    | MISSNNG_TYPE String
    -- typeName instanceName
    | MISSING_INSTANCE String String
    -- typeName instanceName typeOfInsance
    | TYPE_OF_INSTANCE_CHANGED String String TypeOfInstance
    -- typeName instanceName fieldName
    | MISSING_FIELD_IN_INSTANCE String String String
    deriving (Eq,Show, Generic,FromJSON,ToJSON)