{-# LANGUAGE TemplateHaskell #-}
module UmuReactBasic.Templates
  ( indexHtmlFile
  , srcMainFile
  , titleComponentFile
  , spagoDhallFile
  ) where

import           Import
import           UmuReactBasic.TH

indexHtmlFile :: Text
indexHtmlFile = $(embedFileUtf8 "templates/index.html")

srcMainFile :: Text
srcMainFile = $(embedFileUtf8 "templates/SrcMain.purs")

titleComponentFile :: Text
titleComponentFile = $(embedFileUtf8 "templates/TitleComponent.purs")

spagoDhallFile :: Text
spagoDhallFile = $(embedFileUtf8 "templates/spago.dhall")
