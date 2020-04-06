{-# LANGUAGE TemplateHaskell #-}
module UmuReactBasic.Templates
  ( indexHtmlFile
  , srcMainFile
  ) where

import           Import
import           UmuReactBasic.TH

indexHtmlFile :: Text
indexHtmlFile = $(embedFileUtf8 "templates/index.html")

srcMainFile :: Text
srcMainFile = $(embedFileUtf8 "templates/SrcMain.purs")
