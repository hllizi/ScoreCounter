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
        elAttr "meta" ("name"=:"viewport" <> "content" =:"width=device-width, initial-scale=1.0") blank
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
    theWidget = scoreBoard dPlayers . settingsInitialHp <$> dSettings

settingsWidget ::
  MonadWidget t m =>
  m (Event t Settings)
settingsWidget =
  elClass "div" "settings-box" $ mdo
    let inputConfig =
          def
    (dHealth, dNumberOfPlayers) <-
      elClass "div" "quantity-controls" $ mdo
        dHealth <- healthWidget
        dNumberOfPlayers <- numberOfPlayersWidget
        pure (dHealth, dNumberOfPlayers)
    eCreatePlayers <- createButtonWidget

    -- Input fields for the player names
    dPlayers <- elAttr "div" (idAttr "player-settings") $ mdo
      dPlayersRaw <- elAttr "div" (idAttr "player-names") $ mdo
        dPlayers <- widgetHold (pure []) ePlayerCreation
        let inputWidgets = playerNameInputWidgets inputConfig <$> dNumberOfPlayers
        let ePlayerCreation = tag (current inputWidgets) eCreatePlayers
        pure $ fmap (map value) dPlayers

      dPlayers <- (sequence =<<) <$> holdDyn [] (updated dPlayersRaw)
      pure dPlayers

    let settings = Settings <$> dHealth <*> dPlayers
    -- The button to set up an switch to the score board.
    elClass "div" "button-row bottom" $ do
      eSetToInitial <-
        elClass "div" "button-row" $
          buttonClass
            "centered-button rounded-corners bottom-margin"
            "Set to initial"
      pure $ current settings <@ eSetToInitial
  where
    healthWidget = do
      constEmpty <- holdDyn "" never
      dInitialLabel <- holdDyn "Initial HP: " never
      plusMinus
        (dynText . fmap (T.pack . show))
        (layoutHorizontal "" constEmpty)
        (initialSettings ^. #settingsInitialHp)
        dInitialLabel
    numberOfPlayersWidget = do
      constEmpty <- holdDyn "" never
      dNumberOfPlayersLabel <- holdDyn "Number of players: " never
      plusMinus
        (dynText . fmap (T.pack . show))
        (layoutHorizontal "" constEmpty)
        defaultNumberOfPlayers
        dNumberOfPlayersLabel
    createButtonWidget = do
      elAttr "div" (idAttr "create-button-row") $
        buttonClass
          "centered-button rounded-corners"
          "Create Players"

-- | The start widget is the main Widget that ties everything together
startWidget :: MonadWidget t m => m ()
startWidget = mdo
  let dHeaderText = makeHeaderText <$> dSettingsActive
  let switchToSettings = ffilter id (updated dSettingsActive)
  let switchToScoreBoard = ffilter not (updated dSettingsActive)
  elAttr "div" (idAttr "header") $ dynText dHeaderText
  let dPlayers = settingsPlayers <$> dSettingsAndScoreBoard
  let dHp = settingsInitialHp <$> dSettingsAndScoreBoard
  let eInitialHp = tag (current dHp) eSettingsAndScoreBoard
  let eListOfPlayers = tag (current dPlayers) eSet
  let eSet = () <$ eSettingsAndScoreBoard


  deSettingsAndScoreBoard <-
    widgetHold settingsWidget . leftmost $
      [ settingsWidget <$ switchToSettings,
        scoreBoardWidget 
            dSettingsAndScoreBoard 
            dPlayers 
            eInitialHp 
            eListOfPlayers <$ switchToScoreBoard
      ]

  let eSettingsAndScoreBoard = switchDyn deSettingsAndScoreBoard
  dSettingsAndScoreBoard <- holdDyn initialSettings eSettingsAndScoreBoard
  --Show a link that goes back to the settings page in the footer of the scoreboard
  dSettingsActive <- elAttr "div" (idAttr "footer") $ mdo
    let dSwitchLinkWidget = do
          settingsActive <- dSettingsActive
          pure $ if not settingsActive
            then do 
                  (e, _) <- elAttr' "a" ("href" =: "") $ text "New Scoreboard"
                  pure $ domEvent Click e
            else  pure never
    eeBackToSettings <- dyn dSwitchLinkWidget
    eBackToSettings <- switchHold never eeBackToSettings
    toggle True . leftmost $ [eBackToSettings, eSet]
  pure ()
  where
    makeHeaderText :: Bool -> Text
    makeHeaderText settingsActive
      | settingsActive = "Settings"
      | otherwise = "Scoreboard"

-- A Dynamic list of input elements with the provided configuration inputConfig and as many elements as specified by the provided Dynamic t Int
playerNameInputWidgets ::
  forall t m.
  (DomBuilder t m) =>
  InputElementConfig EventResult t (DomBuilderSpace m) ->
  Int ->
  m [InputElement EventResult (DomBuilderSpace m) t]
playerNameInputWidgets inputConfig dPlayerNumber = do
  mapM inputElement . makePlayerInputConfigs $ dPlayerNumber
  where
    -- make input for player #i
    makePlayerInputConfig :: Int -> InputElementConfig EventResult t (DomBuilderSpace m)
    makePlayerInputConfig i =
      inputConfig
        & inputElementConfig_initialValue .~ ("Player " <> T.pack (show i))
        & inputElementConfig_elementConfig
          . elementConfig_initialAttributes
          .~ ("class" =: "name-field rounded-corners")

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
  let eChange =
        leftmost
          [ 
            (+ 1) <$ ePlus,
            limitedDec <$ eMinus
          ]
  (eMinus, ePlus) <- layout label (numberFormat dValue)
  dValue <- foldDyn ($) init eChange
  pure dValue
  where
    limitedDec x
      | x > 0 = x - 1
      | otherwise = x

-- display +/- controls (horizontally arranged)
layoutHorizontal ::
  MonadWidget t m =>
  Text ->
  Dynamic t Text ->
  Dynamic t Text ->
  m a ->
  m (Event t (), Event t ())
layoutHorizontal classes dLabelStyle label number =  mdo
  elClass "div" ("quantity-row" <> " " <> classes) $ do
    elDynClass "span" dLabelStyle $
        dynText label
    elClass "div" "controls" $ do
      eMinus <- buttonClass "button left-button rounded-corners" "-"
      _ <- elClass "span" "number-display" number
      ePlus <- buttonClass "button right-button rounded-corners" "+"
      pure (eMinus, ePlus)

-- display +/- controls vertically arranged
layoutVertical :: MonadWidget t m => Dynamic t Text -> m a -> m (Event t (), Event t ())
layoutVertical label number = mdo
  elClass "div" "large" $ do
    elClass "tr" "player-container" $ mdo
      elClass "td" "player-name" $ dynText label
      eMinus <- elClass "td" "player-minus" $ button "-"
      _ <- elClass "td" "player-number large" number
      ePlus <- elClass "td" "player-plus" $ button "+"
      pure (eMinus, ePlus)

-- Display the players in a list, with scores and controls to modifiy them
scoreBoard ::
  (MonadWidget t m, PostBuild t m, MonadHold t m, MonadFix m, DomBuilder t m) =>
  Dynamic t [Text] ->
  Int ->
  m ()
scoreBoard players initialHp = 
 elAttr "div" (idAttr "scoreboard") $ mdo
  list <-
    simpleList
      players
      displayRow 
  let list2 = sequence <$> list
  _ <- pure $ join list2
  pure ()
  where
    displayRow label = mdo
      dCurrentHp <-
        plusMinus
          (formatHp initialHp)
          (layoutHorizontal "player-row" dLabelStyle)
          initialHp
          label
      dLabelStyle <- holdDyn "" $ makeLabelStyle <$> updated dCurrentHp
      pure dCurrentHp
    makeLabelStyle :: Int -> Text
    makeLabelStyle hp
        | hp == 0 = "dead"
        | otherwise = ""

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

--  formatted HP
formatHp initial dPlayer = mdo
  let dPlayerClass = playerHealthClass initial dPlayer
  elDynClass "span" dPlayerClass $ dynText $ T.pack . show <$> dPlayer

idAttr :: Text -> Map Text Text
idAttr = ("id" =:)

