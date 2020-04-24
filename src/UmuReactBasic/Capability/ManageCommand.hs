{-# LANGUAGE LambdaCase #-}
module UmuReactBasic.Capability.ManageCommand
  ( ManageCommand (..)
  , generateProj
  ) where

import           Import
-- Turtle
import qualified Turtle
import qualified Turtle.Prelude                      as TP
-- Umu
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
  writeAssetsDir mLoc
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
  res <- liftIO
    $ tryJust ( guard . isAlreadyExistsError )
    $ TP.mkdir ( Turtle.fromText $ mkPathName mLoc "src" )
  either
    ( const $ logError "src directory already exists!" )
    ( const $ logInfo "Generating src..." )
    res

writeAssetsDir :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeAssetsDir mLoc = do
  res <- liftIO
    $ tryJust ( guard . isAlreadyExistsError )
    $ TP.mkdir ( Turtle.fromText $ mkPathName mLoc "assets" )
  either
    ( const $ logError "html directory already exists!" )
    ( const $ logInfo "Generating assets..." )
    res

writeIndexHtml :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeIndexHtml mLoc = do
  isExists <- TP.testfile $ Turtle.fromText $ mkPathName mLoc "assets/index.html"
  if isExists
    then logError "assets/index.html already exists!"
    else do
      liftIO $ TP.writeTextFile
        ( Turtle.fromText $ mkPathName mLoc "assets/index.html" )
        indexHtmlFile
      logInfo "Generating assets/index.html..."

writeSrcMainFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeSrcMainFile mLoc = do
  isExists <- TP.testfile $ Turtle.fromText $ mkPathName mLoc "src/Main.purs"
  if isExists
    then logError "src/Main.purs already exists!"
    else do
      liftIO $ TP.writeTextFile
        ( Turtle.fromText $ mkPathName mLoc "/src/Main.purs" ) srcMainFile
      logInfo "Generating src/Main.purs..."

writeComponentDir :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeComponentDir mLoc = do
  res <- liftIO
    $ tryJust ( guard . isAlreadyExistsError )
    $ TP.mkdir ( Turtle.fromText $ mkPathName mLoc "src/Component" )
  either
    ( const $ logError "src/Component directory already exists!" )
    ( const $ logInfo "Generating Component..." )
    res

writeTitleComponentFile  :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeTitleComponentFile mLoc = do
  isExists <- TP.testfile $ Turtle.fromText $ mkPathName mLoc "src/Component/Title.purs"
  if isExists
    then logError "src/Component/Title.purs already exists!"
    else do
      liftIO $ TP.writeTextFile
        ( Turtle.fromText $ mkPathName mLoc "src/Component/Title.purs" )
        titleComponentFile
      logInfo "Generating src/Component/Title.purs..."

writeSpagoDhallFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeSpagoDhallFile mLoc =  do
  isExists <- TP.testfile $ Turtle.fromText $ mkPathName mLoc "spago.dhall"
  if isExists
    then logError "spago.dhall already exists!"
    else do
      liftIO $ TP.writeTextFile
        ( Turtle.fromText $ mkPathName mLoc "spago.dhall" ) spagoDhallFile
      logInfo "Generating spago.dhall..."

writePackagesDhallFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writePackagesDhallFile mLoc = do
  isExists <- TP.testfile $ Turtle.fromText $ mkPathName mLoc "packages.dhall"
  if isExists
    then logError "packages.dhall already exists!"
    else do
      liftIO $
        TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc "packages.dhall" ) packagesDhallFile
      logInfo "Generating packages.dhall..."

writeTestDir :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeTestDir mLoc = do
  res <- liftIO $ tryJust (guard . isAlreadyExistsError) $ TP.mkdir ( Turtle.fromText $ mkPathName mLoc "test" )
  either
    ( const $ logError "test directory already exists!")
    ( const $ logInfo "Generating test..")
    res

writeTestMainFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeTestMainFile mLoc = do
  isExists <- TP.testfile $ Turtle.fromText $ mkPathName mLoc "test/Main.purs"
  if isExists
    then logError "test/Main.purs already exists!"
    else do
      liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc "test/Main.purs" ) testMainFile
      logInfo "Generating test/Main.purs..."

writeMakefile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeMakefile mLoc = do
  isExists <- TP.testfile $ Turtle.fromText $ mkPathName mLoc "Makefile"
  if isExists
    then logError "Makefile already exists!"
    else do
      liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc "Makefile" ) makeFile
      logInfo "Generating Makefile..."

writePackageJsonFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writePackageJsonFile mLoc = do
  isExists <- TP.testfile $ Turtle.fromText $ mkPathName mLoc "package.json"
  if isExists
    then logError "package.json already exists!"
    else do
      liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc "package.json" ) packageJsonFile
      logInfo "Generating package.json..."
