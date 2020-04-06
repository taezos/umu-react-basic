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
  writeComponentDir mLoc
  writeTitleComponentFile mLoc
  writeSpagoDhallFile mLoc
  writePackagesDhallFile mLoc
  writeTestDir mLoc
  writeTestMainFile mLoc
  writeMakefile mLoc

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
    ( Turtle.fromText $ mkPathName mLoc "html/index.html" ) indexHtmlFile
  mkMessage "Generating html/index.html..."

writeSrcMainFile :: MonadIO m => Maybe Text -> m ()
writeSrcMainFile mLoc = do
  liftIO $ TP.writeTextFile
    ( Turtle.fromText $ mkPathName mLoc "/src/Main.purs" ) srcMainFile
  mkMessage "Generating src/Main.purs..."

writeComponentDir :: MonadIO m => Maybe Text -> m ()
writeComponentDir mLoc = do
  liftIO $ TP.mkdir ( Turtle.fromText $ mkPathName mLoc "src/Component" )
  mkMessage "Generating Component..."

writeTitleComponentFile :: MonadIO m => Maybe Text -> m ()
writeTitleComponentFile mLoc = do
  liftIO $ TP.writeTextFile
    ( Turtle.fromText $ mkPathName mLoc "src/Component/Title.purs" ) titleComponentFile
  mkMessage "Generating src/Component/Title.purs..."

writeSpagoDhallFile :: MonadIO m => Maybe Text -> m ()
writeSpagoDhallFile mLoc =  do
  liftIO $ TP.writeTextFile
    ( Turtle.fromText $ mkPathName mLoc "spago.dhall" ) spagoDhallFile
  mkMessage "Generating spago.dhall..."

writePackagesDhallFile :: MonadIO m => Maybe Text -> m ()
writePackagesDhallFile mLoc = do
  liftIO $
    TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc "packages.dhall" ) packagesDhallFile
  mkMessage "Generating packages.dhall..."

writeTestDir :: MonadIO m => Maybe Text -> m ()
writeTestDir mLoc = do
  liftIO $ TP.mkdir ( Turtle.fromText $ mkPathName mLoc "test" )
  mkMessage "Generating test..."

writeTestMainFile :: MonadIO m => Maybe Text -> m ()
writeTestMainFile mLoc = do
  liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc "test/Main.purs" ) testMainFile
  mkMessage "Generating test/Main.purs..."

writeMakefile :: MonadIO m => Maybe Text -> m ()
writeMakefile mLoc = do
  liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc "Makefile" ) makeFile
  mkMessage "Generating Makefile..."
