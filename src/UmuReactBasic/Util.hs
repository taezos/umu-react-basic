module UmuReactBasic.Util
  ( mkPathName
  , generateWhenFileNotExists
  , isFileExists
  ) where

import           Import
-- umu-react-basic
import           UmuReactBasic.Capability.Log
--turtle
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
