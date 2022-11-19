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
        el "title" $ text "Obelisk Minimal Example"
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
initialHp = 10

initialNumberOfPlayers :: Int
initialNumberOfPlayers = 2

data HealthState = Good | Ok | Danger | HighDanger

instance Show HealthState where
  show Good = "good"
  show Ok = "ok"
  show Danger = "danger"
  show HighDanger = "high-danger"

button :: MonadWidget t m => Text -> m (Event t ())
button = buttonClass ""

buttonClass ::
  MonadWidget t m =>
  Text ->
  Text ->
  m (Event t ())
--buttonClass _ label = button label

buttonClass cl label = mdo
  (e, _) <-
    elAttr' "button" ("type" =: "button" <> "class" =: ("large " <> cl)) $
      text label
  pure $ domEvent Click e

main :: MonadWidget t m => m ()
main = undefined

mkHidden :: Bool -> Map Text Text
mkHidden True = "hidden" =: ""
mkHidden False = mempty

data Settings = Settings
  { settingsInitialHp :: Int,
    settingsPlayers :: [Text]
  }
  deriving (Generic)

initialSettings = Settings initialHp []

scoreBoardWidgetFull ::
  MonadWidget t m =>
  Dynamic t Settings ->
  Dynamic t [Text] ->
  Event t Int ->
  Event t [Text] ->
  m (Event t Settings)
scoreBoardWidgetFull dSettings dPlayers eInitialHp eListOfPlayers =
  do
    ePostBuild <- getPostBuild
    dyn playersWidget
    pure $ (current dSettings) <@ ePostBuild
  where
    playersWidget = ((scoreBoard layoutVertical dPlayers) . settingsInitialHp) <$> dSettings

settingsWidgetFull ::
  MonadWidget t m =>
  m (Event t Settings)
settingsWidgetFull =
  elClass "div" "settings-box" $ do
    settingsWidget

-- | The start widget is the main Widget that ties everything together
startWidget :: MonadWidget t m => m ()
startWidget = mdo
  ePostBuild <- getPostBuild
  elClass "div" "header" $ text "Settings"

  let dPlayers = settingsPlayers <$> dSettingsAndSetEvent
  let dHp = settingsInitialHp <$> dSettingsAndSetEvent
  let eInitialHp = tagPromptlyDyn dHp eSettingsAndSetEvent
  let eListOfPlayers = tag (current dPlayers) eSet
  let eSet = () <$ eSettingsAndSetEvent

  dSettingsActive <- elClass "div" "page-bottom" $ mdo
    dSettingsActive <- toggle True . leftmost $ [eSet, eBackToSettings]
    let eBackToSettings = domEvent Click e
    let dSwitchLinkText = do
          settingsActive <- dSettingsActive
          if settingsActive
            then "Switch to score board"
            else "Back to Settings"
    (e, _) <- elAttr' "a" ("href" =: "") $ dynText dSwitchLinkText
    pure dSettingsActive

  let switchToSettings = ffilter id (updated dSettingsActive)
  let switchToScoreTable = ffilter not (updated dSettingsActive)

  deSettingsAndSetEvent <-
    widgetHold settingsWidgetFull . leftmost $
      [ settingsWidgetFull <$ switchToSettings,
        scoreBoardWidgetFull dSettingsAndSetEvent dPlayers eInitialHp eListOfPlayers <$ switchToScoreTable
      ]

  let eSettingsAndSetEvent = switchDyn deSettingsAndSetEvent
  dSettingsAndSetEvent <- holdDyn initialSettings eSettingsAndSetEvent
  el "div" $ dynText $ (T.pack . show . settingsInitialHp) <$> dSettingsAndSetEvent
  pure ()

settingsWidget :: MonadWidget t m => m (Event t Settings)
settingsWidget = mdo
  ePostBuild <- getPostBuild
  let eInitPlayernumber = 2 <$ ePostBuild
  let inputConfig =
        def
          & inputElementConfig_elementConfig
            . elementConfig_initialAttributes
            .~ ("class" =: "large centered")
  (dHp, dNumberOfPlayers, eCreatePlayers) <-
    elClass "div" "base-settings" $ mdo
      dHp <- do
        dInitialLabel <- holdDyn "Initial: " never
        plusMinus
          (dynText . fmap (T.pack . show))
          layoutHorizontal
          (initialSettings ^. #settingsInitialHp)
          dInitialLabel

      dNumberOfPlayers <- do
        dNumberOfPlayersLabel <- holdDyn "Number of players: " never
        plusMinus
          (dynText . fmap (T.pack . show))
          layoutHorizontal
          initialNumberOfPlayers 
          dNumberOfPlayersLabel

      eCreatePlayers <-
        elClass "div" "button-row" $
          buttonClass
            "centered-button"
            "Create Players"
      pure (dHp, dNumberOfPlayers, eCreatePlayers)

  -- Inpts for the player names
  dPlayers <- elClass "div" "player-settings" $ mdo
    dPlayersRaw <- elDynClass "div" "player-names" $ mdo
      let widgets = playerWidgets inputConfig dNumberOfPlayers
      let ePlayerCreation = current widgets <@ leftmost [eCreatePlayers, ePostBuild]
      dPlayers <- widgetHold ((: []) <$> inputElement inputConfig) ePlayerCreation
      pure $ fmap (map value) dPlayers

    ddPlayers <- fmap sequence <$> holdDyn [] (updated dPlayersRaw)
    pure $ join ddPlayers

  let settings = Settings <$> dHp <*> dPlayers
  -- The button to set up an switch to the score board.
  elClass "div" "button-row bottom" $ do
    eSetToInitial <-
      elClass "div" "button-row" $
        buttonClass
          "centered-button"
          "Set to initial"
    pure $ (current settings) <@ eSetToInitial

playerWidgets ::
  (DomBuilder t m, Reflex t) =>
  InputElementConfig EventResult t (DomBuilderSpace m) ->
  Dynamic t Int ->
  Dynamic t (m [InputElement EventResult (DomBuilderSpace m) t])
playerWidgets inputConfig dPlayerNumber = do
  playerNumber <- dPlayerNumber
  pure $ mapM inputElement $ makePlayerInputConfigs playerNumber
  where
    makePlayerInputConfig :: (DomSpace s, Reflex t) => Int -> InputElementConfig EventResult t s
    makePlayerInputConfig i =
      ( def
          & inputElementConfig_elementConfig
            . elementConfig_initialAttributes
          .~ ("class" =: "name-field" <> "placeholder" =: ("Player " <> (T.pack $ show i)))
      )

    makePlayerInputConfigs :: (DomSpace s, Reflex t) => Int -> [InputElementConfig EventResult t s]
    makePlayerInputConfigs numberOfPlayers =
      map
        makePlayerInputConfig
        [1 .. numberOfPlayers]

displayInputLine :: MonadWidget t m => Dynamic t (m (InputElement EventResult (DomBuilderSpace m) t)) -> m ()
displayInputLine dInputLine = mdo
  text "Dynte Baal"
  dyn_ dInputLine

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
  Dynamic t Text ->
  m a ->
  m (Event t (), Event t ())
layoutHorizontal label number = mdo
  elClass "div" "settings-row" $ do
    dynText label
    elClass "span" "controls" $ do
      eMinus <- buttonClass "button left-button" "-"
      elClass "span" "number-display" number
      ePlus <- buttonClass "button right-button" "+"
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
scoreBoard layout players init = mdo
  list <-
    simpleList
      players
      (displayRow init)
  let list2 = sequence <$> list
  pure $ join list2
  pure ()
  where
    displayRow = do
      pure $ plusMinus
        (formatHp init)
        layout
        init

-- determine the class to use for the score. (for adjusting colour to the amount of HP)
playerHealthClass max dCurrentHp = do
  currentHp <- dCurrentHp
  pure $ T.pack . show $ healthState max currentHp
  where
    healthState :: Int -> Int -> HealthState
    healthState upper hp
      | hp > 2 * (upper `div` 3) = Good
      | hp > upper `div` 3 = Ok
      | hp < upper `div` 10 = HighDanger
      | otherwise = Danger

formatHp initial dPlayer = mdo
  let dPlayerClass = playerHealthClass initial dPlayer
  elDynClass "span" dPlayerClass $ dynText $ T.pack . show <$> dPlayer
