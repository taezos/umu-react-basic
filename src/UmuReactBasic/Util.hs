module UmuReactBasic.Util
  ( mkPathName
  , mkMessage
  ) where

import           Import
import           System.Console.ANSI as ANSI

mkPathName :: Maybe Text -> Text -> Text
mkPathName mLoc fileName =
  maybe "./" (\loc -> "./" <> loc <> "/") mLoc <> fileName

mkMessage :: MonadIO m => Text -> m ()
mkMessage msg = do
  liftIO $ ANSI.setSGR [ ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Green ]
  putStr "[INFO]: "
  liftIO $ ANSI.setSGR []
  putStrLn msg
