{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types where


import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

newtype Points = Points { unPoints :: Int }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

instance Num Points where
  (Points a) + (Points b) = Points (a + b)
  (Points a) - (Points b) = Points (a - b)
  (Points a) * (Points b) = Points (a * b)
  abs (Points a) = Points (abs a)
  signum (Points a) = Points (signum a)
  fromInteger i = Points (fromInteger i)

data Nation
  = Germany
  | USA
  | UK
  | SovietUnion
  | Japan
  deriving (Eq, Show, Read, Enum, Bounded, Generic, ToJSON, FromJSON)

data SlotType
  = OfficerSlot
  | InfantrySlot
  | WeaponsTeamSlot
  | ArtillerySlot
  | TankSlot
  | TransportSlot
  deriving (Eq, Show, Read, Enum, Bounded, Generic, ToJSON, FromJSON)

data OptionType
  = ToggleOption
  | CountOption Int Int
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data CostRule
  = ToggleCost Points
  | CountCost Points
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data UnitOptionDef = UnitOptionDef
  { optionKey   :: String
  , optionLabel :: String
  , optionType  :: OptionType
  , optionCost  :: CostRule
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data UnitDef = UnitDef
  { unitKey          :: String
  , unitName         :: String
  , unitSlot         :: SlotType
  , unitBaseCost     :: Points
  , unitMinSize      :: Int
  , unitMaxSize      :: Int
  , unitCostPerExtra :: Points
  , unitOptionDefs   :: [UnitOptionDef]
  , unitNations      :: [Nation]
  , unitAllowedIn    :: [String]
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data SelectedOptionValue
  = OptToggle Bool
  | OptCount Int
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data SelectedUnit = SelectedUnit
  { selectedUnitKey :: String
  , selectedSize    :: Int
  , selectedOptions :: [(String, SelectedOptionValue)]
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data PlatoonTemplate = PlatoonTemplate
  { platoonTemplateKey   :: String
  , platoonTemplateName  :: String
  , platoonNationAccess  :: [Nation]
  , platoonRequiredSlots :: [(SlotType, Int)]
  , platoonMaxSlots      :: [(SlotType, Int)]
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data PlatoonInstance = PlatoonInstance
  { platoonInstanceName :: String
  , platoonTemplateRef  :: String
  , platoonUnits        :: [SelectedUnit]
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Roster = Roster
  { rosterName    :: String
  , rosterNation  :: Nation
  , rosterLimit   :: Maybe Points
  , rosterPlatoons :: [PlatoonInstance]
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Severity = VError | VWarning
  deriving (Eq, Show)

data ValidationMessage = ValidationMessage
  { vmSeverity :: Severity
  , vmPath     :: String
  , vmMessage  :: String
  } deriving (Eq, Show)
