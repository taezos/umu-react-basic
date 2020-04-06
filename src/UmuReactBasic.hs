{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module UmuReactBasic where

import           Import
import           Options.Applicative
import           UmuReactBasic.Capability.ManageCommand
import           UmuReactBasic.Parser

newtype AppM m a
  = AppM
  { unAppM :: m a
  } deriving ( Functor, Applicative, Monad, MonadIO )

startApp :: IO ()
startApp = do
  comm <- showHelpOnErrorExecParser
    ( info ( helper <*> parseVersion <*> parseCommand )
      ( fullDesc <> progDesc umuProgDesc <> header umuHeader ))
  runAppM $ run comm
  where
    run :: MonadIO m => ManageCommand m => Command -> AppM m ()
    run comm = case comm of
      CommandInit mLoc -> do
        generateProject mLoc

instance MonadIO m => ManageCommand ( AppM m ) where
  generateProject = generateProj

runAppM :: MonadIO m => AppM m a -> m a
runAppM app = unAppM app

umuProgDesc :: String
umuProgDesc = "Use umu-react-basic to generate a scaffold "
  <> "for a react-basic-hooks project"

umuHeader :: String
umuHeader = "umu-react-basic: Generate react-basic-hooks Project"

showHelpOnErrorExecParser :: ParserInfo a -> IO a
showHelpOnErrorExecParser = customExecParser ( prefs showHelpOnError )
