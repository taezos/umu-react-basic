{-# LANGUAGE TemplateHaskell #-}
module UmuReactBasic.TH
  ( embedFileUtf8
  ) where

import           Data.FileEmbed
import qualified Data.Text.Encoding         as TE
import           Import
import           Language.Haskell.TH.Syntax (Exp, Q)

embedFileUtf8 :: FilePath -> Q Exp
embedFileUtf8 filePath =
  [| TE.decodeUtf8 $( makeRelativeToProject filePath >>= embedFile ) |]
