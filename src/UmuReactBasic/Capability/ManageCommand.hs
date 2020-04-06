module UmuReactBasic.Capability.ManageCommand
  ( ManageCommand (..)
  , generateProj
  ) where

import           Import
import qualified Turtle
import qualified Turtle.Prelude                      as TP
import           UmuReactBasic.Capability.LogMessage
import           UmuReactBasic.Templates
import           UmuReactBasic.Util

class Monad m => ManageCommand m where
  generateProject :: Maybe Text -> m ()

instance ManageCommand IO where
  generateProject = liftIO . generateProject

-- ManageCommand constraint is just so this function can only be used if
-- there's an instance of ManageCommand
generateProj
  :: ( MonadIO m, ManageCommand m, LogMessage m )
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
  writePackageJsonFile mLoc

writeSrcDir :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeSrcDir mLoc = do
  liftIO $ TP.mkdir ( Turtle.fromText $ mkPathName mLoc "src" )
  logInfo "Generating src..."

writeHtmlDir :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeHtmlDir mLoc = do
  liftIO $ TP.mkdir ( Turtle.fromText $ mkPathName mLoc "html" )
  logInfo "Generating html..."

writeIndexHtml :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeIndexHtml mLoc = do
  liftIO $ TP.writeTextFile
    ( Turtle.fromText $ mkPathName mLoc "html/index.html" ) indexHtmlFile
  logInfo "Generating html/index.html..."

writeSrcMainFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeSrcMainFile mLoc = do
  liftIO $ TP.writeTextFile
    ( Turtle.fromText $ mkPathName mLoc "/src/Main.purs" ) srcMainFile
  logInfo "Generating src/Main.purs..."

writeComponentDir :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeComponentDir mLoc = do
  liftIO $ TP.mkdir ( Turtle.fromText $ mkPathName mLoc "src/Component" )
  logInfo "Generating Component..."

writeTitleComponentFile  :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeTitleComponentFile mLoc = do
  liftIO $ TP.writeTextFile
    ( Turtle.fromText $ mkPathName mLoc "src/Component/Title.purs" ) titleComponentFile
  logInfo "Generating src/Component/Title.purs..."

writeSpagoDhallFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeSpagoDhallFile mLoc =  do
  liftIO $ TP.writeTextFile
    ( Turtle.fromText $ mkPathName mLoc "spago.dhall" ) spagoDhallFile
  logInfo "Generating spago.dhall..."

writePackagesDhallFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writePackagesDhallFile mLoc = do
  liftIO $
    TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc "packages.dhall" ) packagesDhallFile
  logInfo "Generating packages.dhall..."

writeTestDir :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeTestDir mLoc = do
  liftIO $ TP.mkdir ( Turtle.fromText $ mkPathName mLoc "test" )
  logInfo "Generating test..."

writeTestMainFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeTestMainFile mLoc = do
  liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc "test/Main.purs" ) testMainFile
  logInfo "Generating test/Main.purs..."

writeMakefile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeMakefile mLoc = do
  liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc "Makefile" ) makeFile
  logInfo "Generating Makefile..."

writePackageJsonFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writePackageJsonFile mLoc = do
  liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc "package.json" ) packageJsonFile
  logInfo "Generating package.json..."
