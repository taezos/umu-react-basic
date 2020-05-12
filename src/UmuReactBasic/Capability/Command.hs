module UmuReactBasic.Capability.Command
  ( ManageCommand (..)
  , generateProj
  ) where

import           Import
-- Turtle
import qualified Turtle
import qualified Turtle.Prelude               as TP
-- Umu
import           UmuReactBasic.Capability.Log
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
baseGeneration mPathInput = do
  traverse_ ( $ mPathInput )
    [ writeSrcDir
    , writeAssetsDir
    , writeIndexHtml
    , writeHotRelodingIndexJs
    , writeSrcMainFile
    , writeComponentDir
    , writeTitleComponentFile
    , writeSpagoDhallFile
    , writePackagesDhallFile
    , writeTestDir
    , writeTestMainFile
    , writeMakefile
    , writePackageJsonFile
    ]

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
writeSrcDir mPathInput = do
  res <- isDirGenerated mPathInput dirName
  dirResHandler dirName res
  where
    dirName :: Text
    dirName = "src"

writeAssetsDir :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeAssetsDir mPathInput = do
  res <- isDirGenerated mPathInput dirName
  dirResHandler dirName res
  where
    dirName :: Text
    dirName = "assets"

writeComponentDir :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeComponentDir mPathInput = do
  res <- isDirGenerated mPathInput dirName
  dirResHandler dirName res
  where
    dirName :: Text
    dirName = "src/Component"

writeTestDir :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeTestDir mPathInput = do
  res <- isDirGenerated mPathInput dirName
  dirResHandler dirName res
  where
    dirName :: Text
    dirName = "test"

------------------------------------------
--- FILE GENERATION
------------------------------------------
writeIndexHtml :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeIndexHtml mPathInput = do
  isExists <- isFileExists mPathInput filePath
  generateWhenFileNotExists isExists mPathInput filePath indexHtmlFile
  where
    filePath :: Text
    filePath = "assets/index.html"

writeSrcMainFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeSrcMainFile mPathInput = do
  isExists <- isFileExists mPathInput filePath
  generateWhenFileNotExists isExists mPathInput filePath srcMainFile
  where
    filePath :: Text
    filePath = "src/Main.purs"

writeTitleComponentFile  :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeTitleComponentFile mPathInput = do
  isExists <- isFileExists mPathInput filePath
  generateWhenFileNotExists isExists mPathInput filePath titleComponentFile
  where
    filePath :: Text
    filePath = "src/Component/Title.purs"

writeSpagoDhallFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeSpagoDhallFile mPathInput =  do
  isExists <- isFileExists mPathInput filePath
  generateWhenFileNotExists isExists mPathInput filePath spagoDhallFile
  where
    filePath :: Text
    filePath = "spago.dhall"

writePackagesDhallFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writePackagesDhallFile mPathInput = do
  isExists <- isFileExists mPathInput filePath
  generateWhenFileNotExists isExists mPathInput filePath packagesDhallFile
  where
    filePath :: Text
    filePath = "packages.dhall"

writeTestMainFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeTestMainFile mPathInput = do
  isExists <- isFileExists mPathInput filePath
  generateWhenFileNotExists isExists mPathInput filePath testMainFile
  where
    filePath :: Text
    filePath = "test/Main.purs"

writeMakefile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeMakefile mPathInput = do
  isExists <- isFileExists mPathInput filePath
  generateWhenFileNotExists isExists mPathInput filePath makeFile
  where
    filePath :: Text
    filePath = "Makefile"

writePackageJsonFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writePackageJsonFile mPathInput = do
  isExists <- isFileExists mPathInput filePath
  generateWhenFileNotExists isExists mPathInput filePath packageJsonFile
  where
    filePath :: Text
    filePath = "package.json"

writeHotRelodingIndexJs :: ( MonadIO m , LogMessage m ) => Maybe Text -> m ()
writeHotRelodingIndexJs mPathInput = do
  isExists <- isFileExists mPathInput filePath
  generateWhenFileNotExists isExists mPathInput filePath hotReloadIndexJS
  where
    filePath :: Text
    filePath = "assets/index.js"
