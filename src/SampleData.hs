module SampleData where

import Types

platoonTemplates :: [PlatoonTemplate]
platoonTemplates =
  [ PlatoonTemplate
      { platoonTemplateKey = "rifle-platoon"
      , platoonTemplateName = "Rifle Platoon"
      , platoonNationAccess = [Germany, USA, UK, SovietUnion, Japan]
      , platoonRequiredSlots =
          [ (OfficerSlot, 1)
          , (InfantrySlot, 2)
          ]
      , platoonMaxSlots =
          [ (OfficerSlot, 1)
          , (InfantrySlot, 5)
          , (WeaponsTeamSlot, 2)
          , (ArtillerySlot, 1)
          , (TankSlot, 1)
          , (TransportSlot, 3)
          ]
      }
  , PlatoonTemplate
      { platoonTemplateKey = "armoured-platoon"
      , platoonTemplateName = "Armoured Platoon"
      , platoonNationAccess = [Germany, USA, UK, SovietUnion, Japan]
      , platoonRequiredSlots =
          [ (TankSlot, 1)
          ]
      , platoonMaxSlots =
          [ (OfficerSlot, 1)
          , (TankSlot, 3)
          , (TransportSlot, 2)
          ]
      }
  ]

sampleUnits :: [UnitDef]
sampleUnits =
  [ UnitDef
      { unitKey = "second-lt"
      , unitName = "Second Lieutenant"
      , unitSlot = OfficerSlot
      , unitBaseCost = 50
      , unitMinSize = 1
      , unitMaxSize = 2
      , unitCostPerExtra = 15
      , unitOptionDefs =
          [ UnitOptionDef "smg" "Give officer an SMG" ToggleOption (ToggleCost 3)
          ]
      , unitNations = [Germany, USA, UK, SovietUnion, Japan]
      , unitAllowedIn = ["rifle-platoon", "armoured-platoon"]
      }
  , UnitDef
      { unitKey = "rifle-squad"
      , unitName = "Rifle Squad"
      , unitSlot = InfantrySlot
      , unitBaseCost = 60
      , unitMinSize = 5
      , unitMaxSize = 12
      , unitCostPerExtra = 10
      , unitOptionDefs =
          [ UnitOptionDef "nco-smg" "NCO has SMG" ToggleOption (ToggleCost 3)
          , UnitOptionDef "extra-smgs" "Extra SMGs" (CountOption 0 2) (CountCost 3)
          , UnitOptionDef "lmg" "Add LMG" ToggleOption (ToggleCost 20)
          ]
      , unitNations = [Germany, USA, UK, SovietUnion, Japan]
      , unitAllowedIn = ["rifle-platoon"]
      }
  , UnitDef
      { unitKey = "mortar-team"
      , unitName = "Medium Mortar Team"
      , unitSlot = WeaponsTeamSlot
      , unitBaseCost = 50
      , unitMinSize = 2
      , unitMaxSize = 3
      , unitCostPerExtra = 15
      , unitOptionDefs =
          [ UnitOptionDef "spotter" "Add spotter" ToggleOption (ToggleCost 10)
          ]
      , unitNations = [Germany, USA, UK, SovietUnion, Japan]
      , unitAllowedIn = ["rifle-platoon"]
      }
  , UnitDef
      { unitKey = "medium-howitzer"
      , unitName = "Medium Howitzer"
      , unitSlot = ArtillerySlot
      , unitBaseCost = 75
      , unitMinSize = 3
      , unitMaxSize = 3
      , unitCostPerExtra = 0
      , unitOptionDefs = []
      , unitNations = [Germany, USA, UK, SovietUnion, Japan]
      , unitAllowedIn = ["rifle-platoon"]
      }
  , UnitDef
      { unitKey = "medium-tank"
      , unitName = "Medium Tank"
      , unitSlot = TankSlot
      , unitBaseCost = 195
      , unitMinSize = 1
      , unitMaxSize = 1
      , unitCostPerExtra = 0
      , unitOptionDefs = []
      , unitNations = [Germany, USA, UK, SovietUnion, Japan]
      , unitAllowedIn = ["rifle-platoon", "armoured-platoon"]
      }
  ]
