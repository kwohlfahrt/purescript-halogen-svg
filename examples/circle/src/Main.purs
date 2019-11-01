module Main where

import Prelude
import Data.Const (Const)
import Data.Maybe (Maybe(..))

import Effect (Effect)

import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML.Events as HE
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)

import Svg.Elements as SE
import Svg.Attributes as SA

data Action = ToggleState 

type State = { on :: Boolean }

initialState :: forall t . t -> State
initialState = const { on: false }

ui :: forall g. H.Component HTML (Const Void) Unit Void g
ui = H.mkComponent { initialState
                   , render
                   , eval: H.mkEval H.defaultEval
                           { handleAction = handleAction }
                   }
  where
  render :: State -> H.ComponentHTML Action () g
  render state =
    SE.svg [SA.viewBox x y w h]
    [ SE.circle
      [ SA.r (if state.on then w/6.0 else w/3.0)
      , SA.fill $ Just (SA.RGB 0 0 100)
      , HE.onClick (const $ Just ToggleState)
      ]
    ]

    where
    h = 150.0
    w = 150.0
    x = -(w / 2.0)
    y = -(h / 2.0)

  handleAction :: Action -> H.HalogenM State Action () Void g Unit
  handleAction ToggleState =
    H.modify_ (\state -> state { on = not state.on })

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI ui unit body
