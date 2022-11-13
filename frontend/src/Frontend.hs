{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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

healthState :: Int -> Int -> HealthState
healthState upper hp
  | hp > 2 * (upper `div` 3) = Good
  | hp > upper `div` 3 = Ok
  | hp < upper `div` 10 = HighDanger
  | otherwise = Danger

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

data Settings = forall t.
  Settings
  { settingsInitialHp :: Int,
    settingsPlayers :: [Text]
  }

dummySettings = Settings 0 []

data State
  = forall t. StateSettings (Dynamic t (Settings, Event t ()))
  | forall t. StateScoreTable (Dynamic t [Int])


scoreTableWidgetFull :: MonadWidget t m => 
                            Dynamic t [Text]
                        ->  Event t Int
                        ->  m (Dynamic t (Settings, Event t ()))
scoreTableWidgetFull dPlayers eSetToInitial = 
        do
          playersWidget
          pure . pure $ (dummySettings, never)
  where playersWidget = players layoutVertical dPlayers eSetToInitial

settingsWidgetFull :: MonadWidget t m => 
                         Dynamic t Bool 
                      -> m (Dynamic t (Settings, Event t ()))
settingsWidgetFull dSettingsActive = 
    elClass "div" "settings-box" $ do
        settingsWidget dSettingsActive

startWidget :: MonadWidget t m => m ()
startWidget = mdo
  elClass "div" "header" $ text "Settings"
  let dPlayers = settingsPlayers . fst <$> dSettingsAndSetEvent
  let dValue = settingsInitialHp . fst <$> dSettingsAndSetEvent
  let eSetToInitial = current dValue <@ eSet
  eSet <- snd <$> sample (current dSettingsAndSetEvent)

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

  ddSettingsAndSetEvent <-
      widgetHold (settingsWidgetFull dSettingsActive) . leftmost $
        [ (settingsWidgetFull dSettingsActive) <$ switchToSettings,
          (scoreTableWidgetFull dPlayers eSetToInitial) <$ switchToScoreTable
        ]
  let dSettingsAndSetEvent = join ddSettingsAndSetEvent
  pure ()

settingsWidget :: MonadWidget t m => Dynamic t Bool -> m (Dynamic t (Settings, Event t ()))
settingsWidget dSettingsActive = mdo
  ePostBuild <- getPostBuild
  let eInitPlayernumber = 2 <$ ePostBuild
  settingsBox dSettingsActive ePostBuild

settingsBox ::
  MonadWidget t m =>
  Dynamic t Bool ->
  Event t () ->
  m (Dynamic t (Settings, Event t ()))
settingsBox dSettingsActive ePostBuild = mdo
  let inputConfig =
        def
          & inputElementConfig_elementConfig
            . elementConfig_initialAttributes
            .~ ("class" =: "large centered")
  (dValue, dNumberOfPlayers, eCreatePlayers) <-
    elClass "div" "base-settings" $ mdo
      dValue <- do
        dInitialLabel <- holdDyn "Initial: " never
        plusMinus
          (dynText . fmap (T.pack . show))
          layout
          (initialHp <$ ePostBuild)
          dInitialLabel

      dNumberOfPlayers <- do
        dNumberOfPlayersLabel <- holdDyn "Number of players: " never
        plusMinus
          (dynText . fmap (T.pack . show))
          layout
          (initialNumberOfPlayers <$ ePostBuild)
          dNumberOfPlayersLabel

      eCreatePlayers <-
        elClass "div" "button-row" $
          buttonClass
            "centered-button"
            "Create Players"
      pure (dValue, dNumberOfPlayers, eCreatePlayers)

  dPlayers <- elClass "div" "player-settings" $ mdo
    dPlayersRaw <- elDynClass "div" "player-names" $ mdo
      let widgets = playerWidgets inputConfig dNumberOfPlayers
      let ePlayerCreation = current widgets <@ leftmost [eCreatePlayers, ePostBuild]
      dPlayers <- widgetHold ((: []) <$> inputElement inputConfig) ePlayerCreation
      pure $ fmap (map value) dPlayers

    ddPlayers <- fmap sequence <$> holdDyn [] (updated dPlayersRaw)
    pure $ join ddPlayers

  elClass "div" "button-row bottom" $ do
    eSetToInitial <-
      elClass "div" "button-row" $
        buttonClass
          "centered-button"
          "Set to initial"
    pure $ (,) <$> (Settings <$> dValue <*> dPlayers) <*> pure eSetToInitial

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

plusMinus ::
  (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) =>
  (Dynamic t Int -> m ()) ->
  (Dynamic t Text -> m () -> m (Event t (), Event t ())) ->
  Event t Int ->
  Dynamic t Text ->
  m (Dynamic t Int)
plusMinus numberFormat layout eInitial player = mdo
  let eChange =
        leftmost
          [ const <$> eInitial,
            (+ 1) <$ ePlus,
            ( \x ->
                if x > 0
                  then x - 1
                  else x
            )
              <$ eMinus
          ]
  (eMinus, ePlus) <- layout player (numberFormat dValue)
  dValue <- foldDyn ($) 0 eChange
  pure dValue

layout label number = mdo
  elClass "div" "settings-row" $ do
    dynText label
    elClass "span" "controls" $ do
      eMinus <- buttonClass "button left-button" "-"
      elClass "span" "number-display" number
      ePlus <- buttonClass "button right-button" "+"
      pure (eMinus, ePlus)

--layoutVertical :: (MonadWidget t m, DomBuilder t m) => Text -> m () -> m (Event t (), Event t ())
layoutVertical label number = mdo
  elClass "div" "large" $ do
    elClass "tr" "player-container" $ mdo
      elClass "td" "player-name" $ dynText label
      eMinus <- elClass "td" "player-minus" $ button "-"
      elClass "td" "player-number large" number
      ePlus <- elClass "td" "player-plus" $ button "+"
      pure (eMinus, ePlus)

players ::
  (PostBuild t m, MonadHold t m, MonadFix m, DomBuilder t m) =>
  (Dynamic t Text -> m () -> m (Event t (), Event t ())) ->
  Dynamic t [Text] ->
  Event t Int ->
  -- m (Dynamic t [Int])
  m ()
players layout players eInitial = mdo
  dInitial <- holdDyn initialHp eInitial
  list <-
    simpleList
      players
      (displayRow dInitial)
  let list2 = sequence <$> list
  pure $ join list2
  pure ()
  where
    displayRow dInitial = do
      plusMinus
        (formatHp dInitial)
        layout
        eInitial

playerHealthClass dInitial dPlayer = do
  max <- dInitial
  playerHp <- dPlayer
  pure $ T.pack . show $ healthState max playerHp

formatHp dInitial dPlayer = mdo
  let dPlayerClass = playerHealthClass dInitial dPlayer
  elDynClass "span" dPlayerClass $ dynText $ T.pack . show <$> dPlayer
