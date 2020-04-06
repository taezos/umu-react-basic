module UmuReactBasic.Capability.ManageCommand
  ( ManageCommand (..)
  , generateProj
  ) where

import           Import
import qualified Turtle
import qualified Turtle.Prelude          as TP
import           UmuReactBasic.Templates
import           UmuReactBasic.Util

class Monad m => ManageCommand m where
  generateProject :: Maybe Text -> m ()

instance ManageCommand IO where
  generateProject = liftIO . generateProject

-- ManageCommand constraint is just so this function can only be used if
-- there's an instance of ManageCommand
generateProj
  :: ( MonadIO m, ManageCommand m )
  => Maybe Text
  -> m ()
generateProj mLoc = do
  writeSrcDir mLoc
  writeHtmlDir mLoc
  writeIndexHtml mLoc
  writeSrcMainFile mLoc

writeSrcDir :: MonadIO m => Maybe Text -> m ()
writeSrcDir mLoc = do
  liftIO $ TP.mkdir ( Turtle.fromText $ mkPathName mLoc "src" )
  mkMessage "Generating src..."

writeHtmlDir :: MonadIO m => Maybe Text -> m ()
writeHtmlDir mLoc = do
  liftIO $ TP.mkdir ( Turtle.fromText $ mkPathName mLoc "html" )
  mkMessage "Generating html..."

writeIndexHtml :: MonadIO m => Maybe Text -> m ()
writeIndexHtml mLoc = do
  liftIO $ TP.writeTextFile
    ( Turtle.fromText $ mkPathName mLoc "/html/index.html" ) indexHtmlFile
  mkMessage "Generating html/index.html..."

writeSrcMainFile :: MonadIO m => Maybe Text -> m ()
writeSrcMainFile mLoc = do
  liftIO $ TP.writeTextFile
    ( Turtle.fromText $ mkPathName mLoc "/src/Main.purs" ) srcMainFile
  mkMessage "Generating src/Main.purs..."
