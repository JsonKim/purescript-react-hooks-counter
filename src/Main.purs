module Main where

import Prelude

import Counter (mkApp)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import React.Basic (element)
import React.Basic.DOM (render)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

main :: Effect Unit
main = do
  log "Rendering counter app."

  container <- (toNonElementParentNode <$> (window >>= document)) >>= getElementById "container"
  case container of
    Nothing -> throw "Container element not found."
    Just c -> do
      app <- mkApp
      render (element app {}) c
