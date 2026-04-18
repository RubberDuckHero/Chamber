
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (when)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict', encodeFile)
import Data.List (find, intercalate)
import Data.Maybe (fromMaybe, mapMaybe)
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

import Types
import SampleData

findUnitDef :: String -> Maybe UnitDef
findUnitDef k = find (\u -> unitKey u == k) sampleUnits

findPlatoonTemplate :: String -> Maybe PlatoonTemplate
findPlatoonTemplate k = find (\p -> platoonTemplateKey p == k) platoonTemplates

defaultPlatoon :: PlatoonInstance
defaultPlatoon =
  PlatoonInstance
    { platoonInstanceName = "Rifle Platoon 1"
    , platoonTemplateRef = "rifle-platoon"
    , platoonUnits = []
    }

defaultRoster :: Roster
defaultRoster =
  Roster
    { rosterName = "New Roster"
    , rosterNation = Germany
    , rosterLimit = Just 1000
    , rosterPlatoons = [defaultPlatoon]
    }

mkSelectedUnit :: UnitDef -> SelectedUnit
mkSelectedUnit def =
  SelectedUnit
    { selectedUnitKey = unitKey def
    , selectedSize = unitMinSize def
    , selectedOptions = map mkDefault (unitOptionDefs def)
    }
  where
    mkDefault od =
      case optionType od of
        ToggleOption     -> (optionKey od, OptToggle False)
        CountOption mn _ -> (optionKey od, OptCount mn)

prompt :: String -> IO String
prompt label = do
  putStr label
  putStr " "
  hFlush stdout
  getLine

promptInt :: String -> IO (Maybe Int)
promptInt label = readMaybe <$> prompt label

chooseFromMenu :: String -> [String] -> IO (Maybe Int)
chooseFromMenu title options = do
  putStrLn ""
  putStrLn title
  mapM_ (\(i, s) -> putStrLn $ "  " ++ show i ++ ") " ++ s) (zip [1 :: Int ..] options)
  putStrLn "  0) Cancel"
  choice <- promptInt "Choose:"
  pure $
    case choice of
      Just 0 -> Nothing
      Just n | n >= 1 && n <= length options -> Just (n - 1)
      _ -> Nothing

lookupReq :: SlotType -> [(SlotType, Int)] -> Int
lookupReq slot xs = fromMaybe 0 (lookup slot xs)

setOption :: String -> SelectedOptionValue -> [(String, SelectedOptionValue)] -> [(String, SelectedOptionValue)]
setOption k v [] = [(k, v)]
setOption k v ((k2, v2):xs)
  | k == k2   = (k, v) : xs
  | otherwise = (k2, v2) : setOption k v xs

costForOption :: UnitOptionDef -> SelectedOptionValue -> Points
costForOption od ov =
  case (optionCost od, ov) of
    (ToggleCost p, OptToggle True) -> p
    (ToggleCost _, OptToggle False) -> 0
    (CountCost p, OptCount n) -> fromIntegral n * p
    _ -> 0

unitOptionCost :: UnitDef -> SelectedUnit -> Points
unitOptionCost def su =
  sum
    [ costForOption od (fromMaybe defaultVal (lookup (optionKey od) (selectedOptions su)))
    | od <- unitOptionDefs def
    , let defaultVal =
            case optionType od of
              ToggleOption     -> OptToggle False
              CountOption mn _ -> OptCount mn
    ]

unitPoints :: SelectedUnit -> Either String Points
unitPoints su = do
  def <- maybe (Left $ "Unknown unit: " ++ selectedUnitKey su) Right $
    findUnitDef (selectedUnitKey su)
  let extraModels = max 0 (selectedSize su - unitMinSize def)
  pure $
    unitBaseCost def
      + fromIntegral extraModels * unitCostPerExtra def
      + unitOptionCost def su

platoonPoints :: PlatoonInstance -> Either String Points
platoonPoints p = fmap sum (mapM unitPoints (platoonUnits p))

rosterPoints :: Roster -> Either String Points
rosterPoints r = fmap sum (mapM platoonPoints (rosterPlatoons r))

platoonsForNation :: Nation -> [PlatoonTemplate]
platoonsForNation n =
  filter (\p -> n `elem` platoonNationAccess p) platoonTemplates

unitsForNationAndPlatoon :: Nation -> String -> [UnitDef]
unitsForNationAndPlatoon n platoonKey =
  filter allowed sampleUnits
  where
    allowed u =
      n `elem` unitNations u &&
      platoonKey `elem` unitAllowedIn u

validateRoster :: Roster -> [ValidationMessage]
validateRoster r =
  concatMap validatePlatoon (zip [1 :: Int ..] (rosterPlatoons r))
  ++ validateLimit
  where
    validatePlatoon :: (Int, PlatoonInstance) -> [ValidationMessage]
    validatePlatoon (idx, p) =
      case findPlatoonTemplate (platoonTemplateRef p) of
        Nothing ->
          [ValidationMessage VError path "Unknown platoon template."]
        Just pt ->
          templateAccessCheck pt
          ++ unitChecks pt
          ++ slotChecks pt
      where
        path = "Platoon " ++ show idx

        templateAccessCheck pt =
          [ ValidationMessage VError path "This platoon is not available for the roster nation."
          | rosterNation r `notElem` platoonNationAccess pt
          ]

        unitChecks pt = concatMap (validateUnit path pt) (platoonUnits p)

        validateUnit base pt su =
          case findUnitDef (selectedUnitKey su) of
            Nothing ->
              [ValidationMessage VError base ("Unknown unit: " ++ selectedUnitKey su)]
            Just def ->
              nationCheck def ++ platoonCheck def ++ sizeCheck def ++ optionChecks def
          where
            nationCheck def =
              [ ValidationMessage VError base (unitName def ++ " is not allowed for this nation.")
              | rosterNation r `notElem` unitNations def
              ]

            platoonCheck def =
              [ ValidationMessage VError base (unitName def ++ " is not allowed in " ++ platoonTemplateName pt ++ ".")
              | platoonTemplateKey pt `notElem` unitAllowedIn def
              ]

            sizeCheck def =
              [ ValidationMessage VError base $
                  unitName def ++ " size must be between "
                  ++ show (unitMinSize def) ++ " and " ++ show (unitMaxSize def)
              | selectedSize su < unitMinSize def || selectedSize su > unitMaxSize def
              ]

            optionChecks def =
              concatMap (checkOption base) (unitOptionDefs def)
              where
                checkOption b od =
                  case (optionType od, lookup (optionKey od) (selectedOptions su)) of
                    (ToggleOption, Just (OptToggle _)) -> []
                    (ToggleOption, _) ->
                      [ValidationMessage VError b ("Missing toggle option: " ++ optionLabel od)]
                    (CountOption mn mx, Just (OptCount n))
                      | n < mn || n > mx ->
                          [ValidationMessage VError b $
                            optionLabel od ++ " must be between "
                            ++ show mn ++ " and " ++ show mx]
                      | otherwise -> []
                    (CountOption _ _, _) ->
                      [ValidationMessage VError b ("Missing count option: " ++ optionLabel od)]

        slotChecks pt =
          concatMap checkRequired (platoonRequiredSlots pt)
          ++ concatMap checkMax (platoonMaxSlots pt)
          where
            countSlot slot =
              length
                [ ()
                | su <- platoonUnits p
                , Just def <- [findUnitDef (selectedUnitKey su)]
                , unitSlot def == slot
                ]

            checkRequired (slot, req) =
              [ ValidationMessage VError path $
                  "Requires at least " ++ show req ++ " " ++ show slot ++ " unit(s), but has " ++ show (countSlot slot) ++ "."
              | countSlot slot < req
              ]

            checkMax (slot, mx) =
              [ ValidationMessage VError path $
                  "Allows at most " ++ show mx ++ " " ++ show slot ++ " unit(s), but has " ++ show (countSlot slot) ++ "."
              | countSlot slot > mx
              ]

    validateLimit =
      case (rosterLimit r, rosterPoints r) of
        (Just lim, Right total)
          | total > lim ->
              [ValidationMessage VWarning "Roster" $
                "Points limit exceeded: " ++ show (unPoints total) ++ " / " ++ show (unPoints lim)]
        _ -> []

printRoster :: Roster -> IO ()
printRoster r = do
  putStrLn ""
  putStrLn "=============================================================="
  putStrLn $ "Roster: " ++ rosterName r
  putStrLn $ "Nation: " ++ show (rosterNation r)
  putStrLn $ "Limit:  " ++ maybe "None" (show . unPoints) (rosterLimit r)
  putStrLn "--------------------------------------------------------------"

  mapM_ printPlatoon (zip [1 :: Int ..] (rosterPlatoons r))

  putStrLn "--------------------------------------------------------------"
  case rosterPoints r of
    Left err -> putStrLn $ "Total: ERROR - " ++ err
    Right pts -> putStrLn $ "Total Points: " ++ show (unPoints pts)

  let msgs = validateRoster r
  if null msgs
    then putStrLn "Validation: OK"
    else do
      putStrLn "Validation:"
      mapM_ printValidation msgs

  putStrLn "=============================================================="
  putStrLn ""

printPlatoon :: (Int, PlatoonInstance) -> IO ()
printPlatoon (idx, p) = do
  let templateName =
        maybe (platoonTemplateRef p) platoonTemplateName $
          findPlatoonTemplate (platoonTemplateRef p)

  putStrLn $ show idx ++ ". " ++ platoonInstanceName p ++ " [" ++ templateName ++ "]"
  case platoonPoints p of
    Left _ -> pure ()
    Right pts -> putStrLn $ "   Platoon Points: " ++ show (unPoints pts)

  if null (platoonUnits p)
    then putStrLn "   (no units)"
    else mapM_ printUnitLine (zip [1 :: Int ..] (platoonUnits p))

printUnitLine :: (Int, SelectedUnit) -> IO ()
printUnitLine (idx, su) =
  case findUnitDef (selectedUnitKey su) of
    Nothing ->
      putStrLn $ "   " ++ show idx ++ ". " ++ selectedUnitKey su ++ " [UNKNOWN]"
    Just def -> do
      let opts = renderOptions (selectedOptions su)
      let pts = either (const "?") (show . unPoints) (unitPoints su)
      putStrLn $
        "   " ++ show idx ++ ". "
        ++ unitName def
        ++ " | slot=" ++ show (unitSlot def)
        ++ " | size=" ++ show (selectedSize su)
        ++ " | options=" ++ opts
        ++ " | pts=" ++ pts

renderOptions :: [(String, SelectedOptionValue)] -> String
renderOptions [] = "-"
renderOptions xs = intercalate ", " (map go xs)
  where
    go (k, OptToggle b) = k ++ "=" ++ show b
    go (k, OptCount n)  = k ++ "=" ++ show n

printValidation :: ValidationMessage -> IO ()
printValidation vm =
  putStrLn $ "  [" ++ show (vmSeverity vm) ++ "] " ++ vmPath vm ++ ": " ++ vmMessage vm

-- Roster actions

renameRoster :: Roster -> IO Roster
renameRoster r = do
  name <- prompt "New roster name:"
  pure r { rosterName = name }

setNation :: Roster -> IO Roster
setNation r = do
  let nations = [minBound .. maxBound] :: [Nation]
  mIx <- chooseFromMenu "Select nation" (map show nations)
  pure $
    case mIx of
      Nothing -> r
      Just ix -> r { rosterNation = nations !! ix }

setLimit :: Roster -> IO Roster
setLimit r = do
  mVal <- promptInt "Points limit (invalid for none):"
  pure $
    case mVal of
      Just n  -> r { rosterLimit = Just (Points n) }
      Nothing -> r { rosterLimit = Nothing }

addPlatoon :: Roster -> IO Roster
addPlatoon r = do
  let available = platoonsForNation (rosterNation r)
  mIx <- chooseFromMenu "Add platoon" (map platoonTemplateName available)
  pure $
    case mIx of
      Nothing -> r
      Just ix ->
        let pt = available !! ix
            n = length (filter (\p -> platoonTemplateRef p == platoonTemplateKey pt) (rosterPlatoons r)) + 1
            p =
              PlatoonInstance
                { platoonInstanceName = platoonTemplateName pt ++ " " ++ show n
                , platoonTemplateRef = platoonTemplateKey pt
                , platoonUnits = []
                }
        in r { rosterPlatoons = rosterPlatoons r ++ [p] }

removePlatoon :: Roster -> IO Roster
removePlatoon r = do
  if null (rosterPlatoons r)
    then pure r
    else do
      mIx <- chooseFromMenu "Remove which platoon?"
        [ platoonInstanceName p | p <- rosterPlatoons r ]
      pure $
        case mIx of
          Nothing -> r
          Just ix ->
            let xs = rosterPlatoons r
            in r { rosterPlatoons = take ix xs ++ drop (ix + 1) xs }

renamePlatoon :: Roster -> IO Roster
renamePlatoon r = do
  if null (rosterPlatoons r)
    then pure r
    else do
      mIx <- chooseFromMenu "Rename which platoon?"
        [ platoonInstanceName p | p <- rosterPlatoons r ]
      case mIx of
        Nothing -> pure r
        Just ix -> do
          nm <- prompt "New platoon name:"
          let xs = rosterPlatoons r
              p = xs !! ix
              p' = p { platoonInstanceName = nm }
          pure r { rosterPlatoons = take ix xs ++ [p'] ++ drop (ix + 1) xs }

-- Unit actions within a platoon

choosePlatoonIndex :: Roster -> IO (Maybe Int)
choosePlatoonIndex r =
  if null (rosterPlatoons r)
    then do
      putStrLn "No platoons."
      pure Nothing
    else chooseFromMenu "Choose platoon" $
      [ platoonInstanceName p ++ " [" ++ platoonTemplateRef p ++ "]"
      | p <- rosterPlatoons r
      ]

addUnitToPlatoon :: Roster -> IO Roster
addUnitToPlatoon r = do
  mpIx <- choosePlatoonIndex r
  case mpIx of
    Nothing -> pure r
    Just pIx -> do
      let platoon = rosterPlatoons r !! pIx
          available = unitsForNationAndPlatoon (rosterNation r) (platoonTemplateRef platoon)

      if null available
        then do
          putStrLn "No units available for that platoon."
          pure r
        else do
          mIx <- chooseFromMenu "Add unit" $
            [ unitName u ++ " [" ++ show (unitSlot u) ++ "]" | u <- available ]
          pure $
            case mIx of
              Nothing -> r
              Just ix ->
                let def = available !! ix
                    su = mkSelectedUnit def
                    p' = platoon { platoonUnits = platoonUnits platoon ++ [su] }
                    xs = rosterPlatoons r
                in r { rosterPlatoons = take pIx xs ++ [p'] ++ drop (pIx + 1) xs }

removeUnitFromPlatoon :: Roster -> IO Roster
removeUnitFromPlatoon r = do
  mpIx <- choosePlatoonIndex r
  case mpIx of
    Nothing -> pure r
    Just pIx -> do
      let p = rosterPlatoons r !! pIx
      if null (platoonUnits p)
        then do
          putStrLn "No units in that platoon."
          pure r
        else do
          mIx <- chooseFromMenu "Remove which unit?" (map showUnitShort $ platoonUnits p)
          pure $
            case mIx of
              Nothing -> r
              Just ix ->
                let us = platoonUnits p
                    p' = p { platoonUnits = take ix us ++ drop (ix + 1) us }
                    xs = rosterPlatoons r
                in r { rosterPlatoons = take pIx xs ++ [p'] ++ drop (pIx + 1) xs }

editUnitInPlatoon :: Roster -> IO Roster
editUnitInPlatoon r = do
  mpIx <- choosePlatoonIndex r
  case mpIx of
    Nothing -> pure r
    Just pIx -> do
      let p = rosterPlatoons r !! pIx
      if null (platoonUnits p)
        then do
          putStrLn "No units in that platoon."
          pure r
        else do
          mIx <- chooseFromMenu "Edit which unit?" (map showUnitShort $ platoonUnits p)
          case mIx of
            Nothing -> pure r
            Just ix -> do
              su' <- editSelectedUnit (platoonUnits p !! ix)
              let us = platoonUnits p
                  p' = p { platoonUnits = take ix us ++ [su'] ++ drop (ix + 1) us }
                  xs = rosterPlatoons r
              pure r { rosterPlatoons = take pIx xs ++ [p'] ++ drop (pIx + 1) xs }

showUnitShort :: SelectedUnit -> String
showUnitShort su =
  case findUnitDef (selectedUnitKey su) of
    Nothing  -> selectedUnitKey su
    Just def -> unitName def ++ " (size " ++ show (selectedSize su) ++ ")"

editSelectedUnit :: SelectedUnit -> IO SelectedUnit
editSelectedUnit su =
  case findUnitDef (selectedUnitKey su) of
    Nothing -> pure su
    Just def -> unitEditLoop def su

unitEditLoop :: UnitDef -> SelectedUnit -> IO SelectedUnit
unitEditLoop def su = do
  putStrLn ""
  putStrLn $ "Editing: " ++ unitName def
  putStrLn $ "Size: " ++ show (selectedSize su)
  putStrLn $ "Options: " ++ renderOptions (selectedOptions su)
  putStrLn "  1) Change size"
  putStrLn "  2) Edit options"
  putStrLn "  0) Back"
  mChoice <- promptInt "Choose:"
  case mChoice of
    Just 1 -> do
      mSize <- promptInt $
        "New size (" ++ show (unitMinSize def) ++ "-" ++ show (unitMaxSize def) ++ "):"
      let su' = maybe su (\n -> su { selectedSize = n }) mSize
      unitEditLoop def su'
    Just 2 -> do
      su' <- editUnitOptions def su
      unitEditLoop def su'
    _ -> pure su

editUnitOptions :: UnitDef -> SelectedUnit -> IO SelectedUnit
editUnitOptions def su =
  if null (unitOptionDefs def)
    then do
      putStrLn "This unit has no options."
      pure su
    else do
      mIx <- chooseFromMenu "Edit which option?"
        [ renderOptionChoice su od | od <- unitOptionDefs def ]
      case mIx of
        Nothing -> pure su
        Just ix -> do
          let od = unitOptionDefs def !! ix
          case optionType od of
            ToggleOption -> do
              let current = fromMaybe (OptToggle False) (lookup (optionKey od) (selectedOptions su))
                  newVal =
                    case current of
                      OptToggle b -> OptToggle (not b)
                      _ -> OptToggle True
              pure su { selectedOptions = setOption (optionKey od) newVal (selectedOptions su) }

            CountOption mn mx -> do
              mN <- promptInt $ optionLabel od ++ " (" ++ show mn ++ "-" ++ show mx ++ "):"
              let newVal = maybe (OptCount mn) OptCount mN
              pure su { selectedOptions = setOption (optionKey od) newVal (selectedOptions su) }

renderOptionChoice :: SelectedUnit -> UnitOptionDef -> String
renderOptionChoice su od =
  optionLabel od ++ " [" ++ current ++ "]"
  where
    current =
      case lookup (optionKey od) (selectedOptions su) of
        Just (OptToggle b) -> show b
        Just (OptCount n)  -> show n
        Nothing            -> "unset"

-- Save/load

saveRoster :: FilePath -> Roster -> IO ()
saveRoster fp r = encodeFile fp r

loadRoster :: FilePath -> IO (Either String Roster)
loadRoster fp = do
  exists <- doesFileExist fp
  if not exists
    then pure $ Left "File does not exist."
    else do
      res <- eitherDecodeFileStrict' fp
      pure $
        case res of
          Left err -> Left err
          Right r  -> Right r

-- Main loop

main :: IO ()
main = do
  putStrLn "Chamber: Bolt Action List Builder"
  putStrLn "=============================="
  mainLoop defaultRoster

mainLoop :: Roster -> IO ()
mainLoop roster = do
  printRoster roster
  putStrLn "Menu"
  putStrLn "  1) Rename roster"
  putStrLn "  2) Set nation"
  putStrLn "  3) Set points limit"
  putStrLn "  4) Add platoon"
  putStrLn "  5) Rename platoon"
  putStrLn "  6) Remove platoon"
  putStrLn "  7) Add unit to platoon"
  putStrLn "  8) Edit unit in platoon"
  putStrLn "  9) Remove unit from platoon"
  putStrLn " 10) Save"
  putStrLn " 11) Load"
  putStrLn " 12) Validate"
  putStrLn "  0) Quit"

  mChoice <- promptInt "Choose:"
  case mChoice of
    Just 1  -> renameRoster roster >>= mainLoop
    Just 2  -> setNation roster >>= mainLoop
    Just 3  -> setLimit roster >>= mainLoop
    Just 4  -> addPlatoon roster >>= mainLoop
    Just 5  -> renamePlatoon roster >>= mainLoop
    Just 6  -> removePlatoon roster >>= mainLoop
    Just 7  -> addUnitToPlatoon roster >>= mainLoop
    Just 8  -> editUnitInPlatoon roster >>= mainLoop
    Just 9  -> removeUnitFromPlatoon roster >>= mainLoop
    Just 10 -> do
      fp <- prompt "Save to file:"
      saveRoster fp roster
      putStrLn "Saved."
      mainLoop roster
    Just 11 -> do
      fp <- prompt "Load from file:"
      loadRoster fp >>= \case
        Left err -> putStrLn ("Load failed: " ++ err) >> mainLoop roster
        Right r  -> putStrLn "Loaded." >> mainLoop r
    Just 12 -> do
      let msgs = validateRoster roster
      if null msgs
        then putStrLn "Roster is valid."
        else mapM_ printValidation msgs
      mainLoop roster
    Just 0 -> putStrLn "Goodbye."
    _      -> mainLoop roster
