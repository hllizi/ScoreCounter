{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
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
        elAttr
            "link"
            ( "rel" =: "script"
                <> "href" =: "/home/dlahm/lib/bootstrap-5.2.2-dist/js/bootstrap.min.js"
            )
            blank

        elAttr "link" ("href" =: $(static "magic.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
#endif
        pure (),
      _frontend_body = do
        prerender (el "div" $ text "No JS") startValue
        pure ()
        --      $ mdo
        --        eInit <- fmap (initialValue <$) getPostBuild
        --        elClass "div" "header" $ do
        --          (dValue, eSetToInitial) <- elClass "div" "set-box" $ do
        --            dValue <-
        --              elClass "div" "choose-value" $
        --                plusMinus (dynText . fmap (T.pack . show)) layout "Initial: " eInit
        --            eSetToInitial <-
        --              elClass "div" "set-value" $
        --                button "Set to initial"
        --            pure (dValue, eSetToInitial)
        --          el "br" blank
        --          elClass "div" "player-container" $
        --             twoPlayers layoutVertical "Bambus" "Bimbus" $ current dValue <@ eSetToInitial
        --        pure ()
    }

initialValue :: Int
initialValue = 20

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
    elAttr' "button" ("type" =: "button" <> "class" =: ("btn large " <> cl)) $
      text label
  pure $ domEvent Click e

main :: MonadWidget t m => m ()
main = undefined

mkHidden :: Bool -> Map Text Text
mkHidden True = "hidden" =: ""
mkHidden False = mempty

startValue :: MonadWidget t m => m ()
startValue = mdo
  eInit <- fmap (initialValue <$) getPostBuild
  (dValue, eSetToInitial, player1Name, player2Name) <- elClass "div" "header" $ do
    elDynAttr "div" (("class" =: "settings" <>) . mkHidden <$> (not <$> dSettingsActive)) $ do
     elDynClass "div" "settings-format" $ do
      dValue <- do
        dynamicLabel <- holdDyn "Initial: " never
        elClass "div" "choose-value" $
          plusMinus (dynText . fmap (T.pack . show)) layout dynamicLabel eInit
      eSetToInitial <- elClass "div" "set-value" $ button "Set to initial"
      (player1Name, player2Name) <- elDynClass "div" "player-names" $ do
        let namefieldConf = def & 
                                         inputElementConfig_elementConfig
                                       . elementConfig_initialAttributes 
                                            .~ ("class" =: "name-field")

        player1Name <- inputElement namefieldConf
        player2Name <- inputElement namefieldConf
        pure (player1Name, player2Name)
      pure (dValue, eSetToInitial, player1Name, player2Name)
  el "br" blank
  eBackToSettings <- elDynAttr "div" (("class" =: "player-container" <>) <$> mkHidden <$> dSettingsActive) $ do
    twoPlayers layoutVertical (value player1Name) (value player2Name) $ current dValue <@ eSetToInitial
    button "Back to settings"
  dSettingsActive <- toggle True . leftmost $ [eSetToInitial, eBackToSettings]
  pure ()

plusMinus ::
  (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) =>
  (Dynamic t Int -> m ()) ->
  (Dynamic t Text -> m () -> m (Event t (), Event t ())) ->
  Dynamic t Text ->
  Event t Int ->
  m (Dynamic t Int)
plusMinus numberFormat layout player eInitial = mdo
  let change =
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
  dValue <- foldDyn ($) initialValue change
  pure dValue

layout label number = mdo
  dynText label
  eMinus <- button "-"
  elClass "span" "large" number
  ePlus <- button "+"
  pure (eMinus, ePlus)

--layoutVertical :: (MonadWidget t m, DomBuilder t m) => Text -> m () -> m (Event t (), Event t ())
layoutVertical label number = mdo
  elClass "div" "player-column" $ mdo
    elClass "div" "player-name" $ dynText label
    ePlus <- elClass "div" "player-plus" $ button "+"
    elClass "div" "player-number large" number
    eMinus <- elClass "div" "player-minus" $ button "-"
    pure (eMinus, ePlus)

twoPlayers ::
  (PostBuild t m, MonadHold t m, MonadFix m, DomBuilder t m) =>
  (Dynamic t Text -> m () -> m (Event t (), Event t ())) ->
  Dynamic t Text ->
  Dynamic t Text ->
  Event t Int ->
  m (Dynamic t Int, Dynamic t Int)
twoPlayers layout player1 player2 eInitial = mdo
  dInitial <- holdDyn initialValue eInitial
  dPlayer1 <-
    plusMinus
      (formatHp dInitial)
      layout
      player1
      eInitial
  dPlayer2 <-
    plusMinus
      (formatHp dInitial)
      layout
      player2
      eInitial
  pure (dPlayer1, dPlayer2)
  where
    playerHealthClass dInitial dPlayer = do
      max <- dInitial
      playerHp <- dPlayer
      pure $ T.pack . show $ healthState max playerHp

    formatHp dInitial dPlayer = mdo
      let dPlayerClass = playerHealthClass dInitial dPlayer
      elDynClass "span" dPlayerClass $ dynText $ T.pack . show <$> dPlayer
