{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend where

--hiding (button)

import Common.Api
import Common.Route
import Control.Applicative ((<$>), (<*>))
import Control.Lens
import Control.Monad
import Control.Monad.Fix (MonadFix)
import Data.Generics.Labels
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Generics hiding (R)
import Language.Javascript.JSaddle (eval, liftJSM)
import Obelisk.Configs
import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route
import Reflex.Dom.Core hiding (button)
import Text.Read (readMaybe)

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend =
  Frontend
    { _frontend_head = do
        el "title" $ text "Scoreboard"
        --        elAttr
        --            "link"
        --            ( "rel" =: "stylesheet"
        --                <> "href" =: "/home/dlahm/Programmfragmente/reflex/magic/magic.css"
        --            )
        --            blank
#ifdef __GHCIDE__
#else
        elAttr "link" ("href" =: $(static "magic.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
--        elAttr "link" ("href" =: $(static "bootstrap/css/*.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
--        elAttr "link" ("href" =: $(static "bootstrap/js/*.js") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
#endif
        pure (),
      _frontend_body = do
        prerender (el "div" $ text "No JS") startWidget
        pure ()
    }

initialHp :: Int
initialHp = 20

defaultNumberOfPlayers :: Int
defaultNumberOfPlayers = 2

initialSettings = Settings initialHp []

data HealthState = Good | Ok | Danger | HighDanger

instance Show HealthState where
  show Good = "good"
  show Ok = "ok"
  show Danger = "danger"
  show HighDanger = "high-danger"

button :: MonadWidget t m => Text -> m (Event t ())
button = buttonClass "rounded-corners"

buttonClass ::
  MonadWidget t m =>
  Text ->
  Text ->
  m (Event t ())

--buttonClass _ label = button label

-- | create a button with class(es) "cl" and label "label"
buttonClass cl label = mdo
  (e, _) <-
    elAttr' "button" ("type" =: "button" <> "class" =: ("large " <> cl)) $
      text label
  pure $ domEvent Click e

main :: MonadWidget t m => m ()
main = undefined

data Settings = Settings
  { settingsInitialHp :: Int,
    settingsPlayers :: [Text]
  }
  deriving (Generic)

scoreBoardWidget ::
  MonadWidget t m =>
  Dynamic t Settings ->
  Dynamic t [Text] ->
  Event t Int ->
  Event t [Text] ->
  m (Event t Settings)
scoreBoardWidget dSettings dPlayers eInitialHp eListOfPlayers =
  do
    ePostBuild <- getPostBuild
    _ <- dyn theWidget
    pure $ current dSettings <@ ePostBuild
  where
    theWidget = scoreBoard (layoutHorizontal "player-row") dPlayers . settingsInitialHp <$> dSettings

settingsWidget ::
  MonadWidget t m =>
  m (Event t Settings)
settingsWidget =
  elClass "div" "settings-box" $ mdo
      ePostBuild <- getPostBuild
      let inputConfig =
            def
      (dHealth, dNumberOfPlayers, eCreatePlayers) <-
        elClass "div" "base-settings" $ mdo
          dHealth <- healthWidget
          dNumberOfPlayers <- numberOfPlayersWidget
          eCreatePlayers <- createButtonWidget
          pure (dHealth, dNumberOfPlayers, eCreatePlayers)

      -- Input fields for the player names
      dPlayers <- elClass "div" "player-settings" $ mdo
        dPlayersRaw <- elDynClass "div" "player-names" $ mdo
          dPlayers <- widgetHold (pure []) ePlayerCreation
          let inputWidgets = playerWidgets inputConfig <$> dNumberOfPlayers
          let ePlayerCreation = tagPromptlyDyn inputWidgets $ leftmost [eCreatePlayers, ePostBuild]
          pure $ fmap (map value) dPlayers

        dPlayers <- (sequence =<<) <$> holdDyn [] (updated dPlayersRaw)
        pure  dPlayers

      let settings = Settings <$> dHealth <*> dPlayers
      -- The button to set up an switch to the score board.
      elClass "div" "button-row bottom" $ do
        eSetToInitial <-
          elClass "div" "button-row" $
            buttonClass
              "centered-button rounded-corners"
              "Set to initial"
        pure $ current settings <@ eSetToInitial
  where
    healthWidget = do
            dInitialLabel <- holdDyn "Initial HP: " never
            plusMinus
              (dynText . fmap (T.pack . show))
              (layoutHorizontal "")
              (initialSettings ^. #settingsInitialHp)
              dInitialLabel
    numberOfPlayersWidget = do 
            dNumberOfPlayersLabel <- holdDyn "Number of players: " never
            plusMinus
              (dynText . fmap (T.pack . show))
              (layoutHorizontal "")
              defaultNumberOfPlayers
              dNumberOfPlayersLabel
    createButtonWidget = do
            elClass "div" "button-row" $
              buttonClass
                "centered-button rounded-corners"
                "Create Players"

-- | The start widget is the main Widget that ties everything together
startWidget :: MonadWidget t m => m ()
startWidget = mdo
  let dHeaderText = makeHeaderText <$> dSettingsActive
  elAttr "div" ("id" =: "header") $ dynText dHeaderText
  

  let dPlayers = settingsPlayers <$> dSettingsAndSetEvent
  let dHp = settingsInitialHp <$> dSettingsAndSetEvent
  let eInitialHp = tag (current dHp) eSettingsAndSetEvent
  let eListOfPlayers = tag (current dPlayers) eSet
  let eSet = () <$ eSettingsAndSetEvent

  let switchToSettings = ffilter id (updated dSettingsActive)
  let switchToScoreTable = ffilter not (updated dSettingsActive)

  deSettingsAndSetEvent <-
    widgetHold settingsWidget . leftmost $
      [ settingsWidget <$ switchToSettings,
        scoreBoardWidget dSettingsAndSetEvent dPlayers eInitialHp eListOfPlayers <$ switchToScoreTable
      ]

  let eSettingsAndSetEvent = switchDyn deSettingsAndSetEvent
  dSettingsAndSetEvent <- holdDyn initialSettings eSettingsAndSetEvent
  dSettingsActive <- elAttr "div" ("id" =: "footer") $ mdo
    dSettingsActive <- toggle True . leftmost $ [eSet, eBackToSettings]
    let eBackToSettings = domEvent Click e
    let dSwitchLinkText = do
          settingsActive <- dSettingsActive
          if settingsActive
            then "Switch to score board"
            else "New Scoreboard"
    (e, _) <- elAttr' "a" ("href" =: "") $ dynText dSwitchLinkText
    pure dSettingsActive
  pure ()
   where
    makeHeaderText :: Bool -> Text
    makeHeaderText settingsActive
     | settingsActive = "Settings"
     | otherwise  = "Scoreboard"

-- A Dynamic list of input elements with the provided configuration inputConfig and as many elements as specified by the provided Dynamic t Int
playerWidgets ::
  forall t m.
  (DomBuilder t m) =>
  InputElementConfig EventResult t (DomBuilderSpace m) ->
  Int ->
  m [InputElement EventResult (DomBuilderSpace m) t]
playerWidgets inputConfig dPlayerNumber = do
  mapM inputElement . makePlayerInputConfigs $ dPlayerNumber
  where
    -- make input for player #i
    makePlayerInputConfig :: Int -> InputElementConfig EventResult t (DomBuilderSpace m)
    makePlayerInputConfig i =
      inputConfig
        & inputElementConfig_initialValue .~ ("Player " <> T.pack (show i))
        & inputElementConfig_elementConfig 
            . elementConfig_initialAttributes .~ ("class" =: "name-field rounded-corners")

    -- make inputs for all players
    makePlayerInputConfigs :: Int -> [InputElementConfig EventResult t (DomBuilderSpace m)]
    makePlayerInputConfigs numberOfPlayers =
      map
        makePlayerInputConfig
        [1 .. numberOfPlayers]

testWidget :: MonadWidget t m => Dynamic t (m ())
testWidget = pure $ do
  inputz <- inputElement def
  dynText (value inputz)

inputElementSource :: MonadWidget t m => m (InputElement EventResult (DomBuilderSpace m) t)
inputElementSource =
  let namefieldConf =
        def
          & inputElementConfig_elementConfig
            . elementConfig_initialAttributes
          .~ ("class" =: "name-field")
   in inputElement namefieldConf

--  Use a function to format a number, a function to layout the parts, a Dynamic t Int to provide the start value and a label and use them to produce +/- controls for adjusting a labeled numerical value
plusMinus ::
  (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) =>
  (Dynamic t Int -> m ()) ->
  (Dynamic t Text -> m () -> m (Event t (), Event t ())) -> -- Function to layout the control element
  Int -> -- Initial value
  Dynamic t Text -> -- Text label
  m (Dynamic t Int)
plusMinus numberFormat layout init label = mdo
  ePostBuild <- getPostBuild
  let eChange =
        leftmost
          [ const init <$ ePostBuild,
            (+ 1) <$ ePlus,
            limitedDec <$ eMinus
          ]
  (eMinus, ePlus) <- layout label (numberFormat dValue)
  dValue <- foldDyn ($) 0 eChange
  pure dValue
  where
    limitedDec x
      | x > 0 = x - 1
      | otherwise = x

-- display +/- controls (horizontally arranged)
layoutHorizontal ::
  (MonadWidget t m, DomBuilder t m) =>
  Text -> 
  Dynamic t Text ->
  m a ->
  m (Event t (), Event t ())
layoutHorizontal classes label number = mdo
  elClass "div" ("quantity-row" <> " " <> classes) $ do
    dynText label
    elClass "div" "controls" $ do
      eMinus <- buttonClass "button left-button rounded-corners" "-"
      elClass "span" "number-display" number
      ePlus <- buttonClass "button right-button rounded-corners" "+"
      pure (eMinus, ePlus)

-- display +/- controls vertically arranged
layoutVertical :: (MonadWidget t m, DomBuilder t m) => Dynamic t Text -> m a -> m (Event t (), Event t ())
layoutVertical label number = mdo
  elClass "div" "large" $ do
    elClass "tr" "player-container" $ mdo
      elClass "td" "player-name" $ dynText label
      eMinus <- elClass "td" "player-minus" $ button "-"
      elClass "td" "player-number large" number
      ePlus <- elClass "td" "player-plus" $ button "+"
      pure (eMinus, ePlus)

-- Display the players in a list, with scores and controls to modifiy them
scoreBoard ::
  (PostBuild t m, MonadHold t m, MonadFix m, DomBuilder t m) =>
  (Dynamic t Text -> m () -> m (Event t (), Event t ())) ->
  Dynamic t [Text] ->
  Int ->
  m ()
scoreBoard layout players initialHp = mdo
  list <-
    simpleList
      players
      (displayRow init)
  let list2 = sequence <$> list
  _ <- pure $ join list2
  pure ()
  where
    displayRow = do
      pure $
        plusMinus
          (formatHp initialHp)
          layout
          initialHp

-- determine the class to use for the score. (for adjusting colour to the amount of HP)
playerHealthClass :: (Functor f) => Int -> f Int -> f Text
playerHealthClass max dCurrentHp = makeClassString <$> dCurrentHp
  where
    makeClassString = T.pack . show . healthState max 
    healthState :: Int -> Int -> HealthState
    healthState upper hp
      | hp > 2 * (upper `div` 3) = Good
      | hp > upper `div` 3 = Ok
      | hp < upper `div` 10 = HighDanger
      | otherwise = Danger

formatHp initial dPlayer = mdo
  let dPlayerClass = playerHealthClass initial dPlayer
  elDynClass "span" dPlayerClass $ dynText $ T.pack . show <$> dPlayer


-- Helpers
--

singleton :: a -> [a]
singleton = (: [])
