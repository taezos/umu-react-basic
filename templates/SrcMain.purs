module Main where

import Prelude ( Unit, bind, map, ($), (=<<), const, pure )
import Data.Maybe ( Maybe(..) )
-- Internal Component
import Component.Title
-- Web
import Web.DOM.NonElementParentNode ( getElementById )
import Web.HTML.HTMLDocument ( toNonElementParentNode )
import Web.HTML.Window ( document )
import Web.HTML ( window )
-- Effect
import Effect ( Effect )
import Effect.Exception ( throw )
-- React
import React.Basic.Hooks ( element, ReactComponent, component, JSX )
import React.Basic.DOM as R

mkMainComponent :: Effect ( ReactComponent {} )
mkMainComponent = do
  title <- mkTitle
  component "Main" $ const React.do
    pure $
      R.div_
      [ element title { text: "Hello, World" }
      ]

-- This is separated so it can be called in the hot reload function.
mainJSX :: Effect JSX
mainJSX = do
  mainComp <- mkMainComponent
  pure $ element mainComp {}

main :: Effect Unit
main = do
  mApp <- getElementById "app" =<< ( map toNonElementParentNode $ document =<< window )
  case mApp of
    Nothing -> throw "App element not found."
    Just app -> do
      mainComponent <- mainJSX
      R.render mainComponent app
