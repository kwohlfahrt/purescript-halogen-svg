module Main where

import Prelude
import Data.Maybe (Maybe(..))

import Control.Monad.Eff (Eff)

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.Util (awaitBody, runHalogenAff)
import Halogen (HalogenEffects)

import Svg.Elements as SE
import Svg.Attributes as SA

data Query a = ToggleState a

type State = { on :: Boolean }

initialState :: State
initialState = { on: false }

ui :: forall g. H.Component State Query g
ui = H.component { render, eval }
  where
  render :: State -> H.ComponentHTML Query
  render state =
    SE.svg [SA.viewBox x y w h]
    [ SE.circle
      [ SA.r (if state.on then w/6.0 else w/3.0)
      , SA.fill $ Just (SA.RGB 0 0 100)
      , HE.onClick (HE.input_ ToggleState)
      ]
    ]

    where
    h = 150.0
    w = 150.0
    x = -(w / 2.0)
    y = -(h / 2.0)

  eval :: Query ~> H.ComponentDSL State Query g
  eval (ToggleState next) = do
    H.modify (\state -> state { on = not state.on })
    pure next

main :: Eff (HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  H.runUI ui initialState body
