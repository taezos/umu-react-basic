module UmuReactBasic.Util
  ( mkPathName
  , generateWhenFileNotExists
  , isFileExists
  , isDirGenerated
  , dirResHandler
  , generateFile
  ) where

import           Import
-- umu-react-basic
import           UmuReactBasic.Capability.Log
-- turtle
import qualified Turtle
import qualified Turtle.Prelude               as TP

mkPathName :: Maybe Text -> Text -> Text
mkPathName mLoc fileName =
  maybe "./" (\loc -> "./" <> loc <> "/") mLoc <> fileName

isFileExists :: MonadIO m => Maybe Text -> Text -> m Bool
isFileExists mPathInput filePath =
  TP.testfile $ Turtle.fromText ( mkPathName mPathInput filePath )

generateWhenFileNotExists
  :: ( MonadIO m, LogMessage m )
  => Bool
  -> Maybe Text
  -> Text
  -> Text
  -> m ()
generateWhenFileNotExists isExists mPathInput filePath file
  | isExists = logError ( filePath <> " already exists!" )
  | otherwise = generateFile mPathInput filePath file

generateFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> Text -> Text -> m ()
generateFile mPathInput filePath file = do
  liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mPathInput filePath ) file
  logInfo ( "Generated " <> filePath )

-- Right is considered the success case here, and means the directory was
-- created. Left will be the error.
-- TODO: This needs to be refactored coz ( Either () () ) doesn't make no damn
-- sense but this will do for now.
isDirGenerated :: MonadIO m => Maybe Text -> Text -> m ( Either () () )
isDirGenerated mPathInput dirName = liftIO
  $ tryJust ( guard . isAlreadyExistsError )
  $ TP.mkdir ( Turtle.fromText $ mkPathName mPathInput dirName )

dirResHandler :: ( MonadIO m, LogMessage m ) => Text -> Either () () -> m ()
dirResHandler dirName res = either
  ( const $ logError $ dirName <> " directory already exists!" )
  ( const $ logInfo $ "Generated " <> dirName )
  res
