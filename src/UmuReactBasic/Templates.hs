{-# LANGUAGE TemplateHaskell #-}
module UmuReactBasic.Templates
  ( indexHtmlFile
  , srcMainFile
  , titleComponentFile
  , spagoDhallFile
  , packagesDhallFile
  , testMainFile
  , makeFile
  , packageJsonFile
  , hotReloadIndexJS
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

packagesDhallFile :: Text
packagesDhallFile = $(embedFileUtf8 "templates/packages.dhall")

testMainFile :: Text
testMainFile = $(embedFileUtf8 "templates/TestMain.purs")

makeFile :: Text
makeFile = $(embedFileUtf8 "templates/Makefile")

packageJsonFile :: Text
packageJsonFile = $(embedFileUtf8 "templates/package.json")

hotReloadIndexJS :: Text
hotReloadIndexJS  = $(embedFileUtf8 "templates/index.js")
