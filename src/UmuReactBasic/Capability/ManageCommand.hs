{-# LANGUAGE ScopedTypeVariables #-}
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
generateProj mLoc = case mLoc of
  Nothing -> baseGeneration mLoc
  Just loc -> do
    writeInitialDir loc
    baseGeneration mLoc

baseGeneration :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
baseGeneration mLoc = do
  writeSrcDir mLoc
  writeAssetsDir mLoc
  writeIndexHtml mLoc
  writeHotRelodingIndexJs mLoc
  writeSrcMainFile mLoc
  writeComponentDir mLoc
  writeTitleComponentFile mLoc
  writeSpagoDhallFile mLoc
  writePackagesDhallFile mLoc
  writeTestDir mLoc
  writeTestMainFile mLoc
  writeMakefile mLoc
  writePackageJsonFile mLoc

------------------------------------------
--- DIRECTORY GENERATION
------------------------------------------

writeInitialDir :: ( MonadIO m, LogMessage m ) => Text -> m ()
writeInitialDir loc = do
  res <- liftIO
    $ tryJust ( guard . isAlreadyExistsError )
    $ TP.mkdir $ Turtle.fromText loc
  either
    ( const $ logWarn warningMessage )
    ( const $ logInfo $ "Generating " <> loc <> "..." )
    res
  where
    warningMessage :: Text
    warningMessage = loc
      <> " already exists but "
      <> appName
      <> " will continue to generate to that directory..."

writeSrcDir :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeSrcDir mLoc = do
  res <- liftIO
    $ tryJust ( guard . isAlreadyExistsError )
    $ TP.mkdir ( Turtle.fromText $ mkPathName mLoc dirName )
  either
    ( const $ logError $ dirName <> " directory already exists!" )
    ( const $ logInfo $ "Generating " <> dirName <> "..." )
    res
  where
    dirName :: Text
    dirName = "src"


writeAssetsDir :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeAssetsDir mLoc = do
  res <- liftIO
    $ tryJust ( guard . isAlreadyExistsError )
    $ TP.mkdir ( Turtle.fromText $ mkPathName mLoc dirName )
  either
    ( const $ logError $ dirName <> " directory already exists!" )
    ( const $ logInfo $ "Generating " <> dirName <> "..." )
    res
  where
    dirName :: Text
    dirName = "assets"

writeComponentDir :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeComponentDir mLoc = do
  res <- liftIO
    $ tryJust ( guard . isAlreadyExistsError )
    $ TP.mkdir ( Turtle.fromText $ mkPathName mLoc dirName )
  either
    ( const $ logError $ dirName <> " directory already exists!" )
    ( const $ logInfo $ "Generating " <> dirName <> "..." )
    res
  where
    dirName :: Text
    dirName = "src/Component"

writeTestDir :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeTestDir mLoc = do
  res <- liftIO $ tryJust (guard . isAlreadyExistsError) $ TP.mkdir ( Turtle.fromText $ mkPathName mLoc dirName )
  either
    ( const $ logError $ dirName <> " directory already exists!")
    ( const $ logInfo $ "Generating " <> dirName <> "...")
    res
  where
    dirName :: Text
    dirName = "test"

------------------------------------------
--- FILE GENERATION
------------------------------------------
writeIndexHtml :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeIndexHtml mLoc = do
  isExists <- TP.testfile $ Turtle.fromText $ mkPathName mLoc filePath
  if isExists
    then logError $ filePath <> " already exists!"
    else do
      liftIO $ TP.writeTextFile
        ( Turtle.fromText $ mkPathName mLoc filePath )
        indexHtmlFile
      logInfo $ "Generating " <> filePath <> "..."
  where
    filePath :: Text
    filePath = "assets/index.html"

writeSrcMainFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeSrcMainFile mLoc = do
  isExists <- TP.testfile $ Turtle.fromText $ mkPathName mLoc filePath
  if isExists
    then logError $ filePath <> " already exists!"
    else do
      liftIO $ TP.writeTextFile
        ( Turtle.fromText $ mkPathName mLoc filePath ) srcMainFile
      logInfo $ "Generating " <> filePath <> "..."
  where
    filePath :: Text
    filePath = "src/Main.purs"

writeTitleComponentFile  :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeTitleComponentFile mLoc = do
  isExists <- TP.testfile $ Turtle.fromText $ mkPathName mLoc filePath
  if isExists
    then logError $ filePath <> " already exists!"
    else do
      liftIO $ TP.writeTextFile
        ( Turtle.fromText $ mkPathName mLoc filePath )
        titleComponentFile
      logInfo $ "Generating " <> filePath <> "..."
  where
    filePath :: Text
    filePath = "src/Component/Title.purs"

writeSpagoDhallFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeSpagoDhallFile mLoc =  do
  isExists <- TP.testfile $ Turtle.fromText $ mkPathName mLoc filePath
  if isExists
    then logError $ filePath <> " already exists!"
    else do
      liftIO $ TP.writeTextFile
        ( Turtle.fromText $ mkPathName mLoc filePath ) spagoDhallFile
      logInfo $ "Generating " <> filePath <> "..."
  where
    filePath :: Text
    filePath = "spago.dhall"

writePackagesDhallFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writePackagesDhallFile mLoc = do
  isExists <- TP.testfile $ Turtle.fromText $ mkPathName mLoc filePath
  if isExists
    then logError $ filePath <> " already exists!"
    else do
      liftIO $
        TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc filePath ) packagesDhallFile
      logInfo $ "Generating " <> filePath <> "..."
  where
    filePath :: Text
    filePath = "packages.dhall"

writeTestMainFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeTestMainFile mLoc = do
  isExists <- TP.testfile $ Turtle.fromText $ mkPathName mLoc filePath
  if isExists
    then logError $ filePath <> " already exists!"
    else do
      liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc filePath ) testMainFile
      logInfo $ "Generating " <> filePath <> "..."
  where
    filePath :: Text
    filePath = "test/Main.purs"

writeMakefile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeMakefile mLoc = do
  isExists <- TP.testfile $ Turtle.fromText $ mkPathName mLoc filePath
  if isExists
    then logError $ filePath <> " already exists!"
    else do
      liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc filePath ) makeFile
      logInfo $ "Generating " <> filePath <> "..."
  where
    filePath :: Text
    filePath = "Makefile"

writePackageJsonFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writePackageJsonFile mLoc = do
  isExists <- TP.testfile $ Turtle.fromText $ mkPathName mLoc filePath
  if isExists
    then logError $ filePath <> " already exists!"
    else do
      liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc filePath ) packageJsonFile
      logInfo $ "Generating " <> filePath <> "..."
  where
    filePath :: Text
    filePath = "package.json"

writeHotRelodingIndexJs :: ( MonadIO m , LogMessage m ) => Maybe Text -> m ()
writeHotRelodingIndexJs mLoc = do
  isExists <- TP.testfile $  Turtle.fromText $ mkPathName mLoc filePath
  if isExists
    then logError $ filePath <> " already exists!"
    else do
      liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc filePath ) hotReloadIndexJS
      logInfo $ "Generating " <> filePath <> "..."
  where
    filePath :: Text
    filePath = "assets/index.js"
