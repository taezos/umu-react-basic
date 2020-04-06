module UmuReactBasic.Util
  ( mkPathName
  ) where

import           Import

mkPathName :: Maybe Text -> Text -> Text
mkPathName mLoc fileName =
  maybe "./" (\loc -> "./" <> loc <> "/") mLoc <> fileName
