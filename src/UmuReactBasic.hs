{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module UmuReactBasic where

import           Import
import           Options.Applicative
import           UmuReactBasic.Capability.Log
import           UmuReactBasic.Capability.Command
import           UmuReactBasic.Log
import           UmuReactBasic.Parser
import Lens.Micro

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
      CommandInit mLoc -> generateProject mLoc
      CommandComponent pathInput name -> generateComponent pathInput name

instance MonadIO m => ManageCommand ( AppM m ) where
  generateProject = generateProjectImpl
  generateComponent = generateComponentImpl

instance MonadIO m => LogMessage ( AppM m ) where
  logMessage l = case l ^. logReason of
    Info -> do
      mkTerminalLog
        ( l ^. logMsg . logMessageText )
        Info
        ( l ^. logMsg . logMessageHeader )
    Debug -> do
      mkTerminalLog
        ( l ^. logMsg . logMessageText )
        Debug
        ( l ^. logMsg . logMessageHeader )
    Error -> do
      mkTerminalLog
        ( l ^. logMsg . logMessageText )
        Error
        ( l ^. logMsg . logMessageHeader )
    Warn -> do
      mkTerminalLog
        ( l ^. logMsg . logMessageText )
        Warn
        ( l ^. logMsg . logMessageHeader )

runAppM :: MonadIO m => AppM m a -> m a
runAppM app = unAppM app

umuProgDesc :: String
umuProgDesc = "Use umu-react-basic to generate a scaffold "
  <> "for a react-basic-hooks project"

umuHeader :: String
umuHeader = "umu-react-basic: Generate react-basic-hooks Project"

showHelpOnErrorExecParser :: ParserInfo a -> IO a
showHelpOnErrorExecParser = customExecParser ( prefs showHelpOnError )
