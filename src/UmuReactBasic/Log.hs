{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module UmuReactBasic.Log
  ( LogReason (..)
  , Log
  , mkLog
  , logMessageText
  , logMessageHeader
  , logReason
  , logMsg
  , mkTerminalLog
  ) where

import           Import
import           Lens.Micro.TH
import           System.Console.ANSI as ANSI

data LogReason
  = Debug
  | Info
  | Warn
  | Error
  deriving ( Eq, Show )

data LogMessage = LogMessage
  { _logMessageText   :: Text
  , _logMessageHeader :: Text
  } deriving ( Show )

makeLenses ''LogMessage

data Log = Log
  { _logReason :: LogReason
  , _logMsg    :: LogMessage
  } deriving ( Show )

makeLenses ''Log

mkLog :: MonadIO m => LogReason -> Text -> m Log
mkLog reason msg = do
  pure $ Log
    { _logReason = reason
    , _logMsg = LogMessage
      { _logMessageText = msg
      , _logMessageHeader = mkHeader reason
      }
    }
  where
    mkHeader :: LogReason -> Text
    mkHeader res = case res of
      Debug -> "[DEBUG]: "
      Info  -> "[INFO]: "
      Warn  -> "[WARN]: "
      Error -> "[ERROR]: "

mkTerminalLog :: MonadIO m => Text -> LogReason -> Text -> m ()
mkTerminalLog msg reason logHeader = do
  liftIO $ ANSI.setSGR [ ANSI.SetColor ANSI.Foreground ANSI.Dull ( reasonToColor reason )]
  putStr logHeader
  liftIO $ ANSI.setSGR []
  putStrLn msg
  where
    reasonToColor :: LogReason -> Color
    reasonToColor lr = case lr of
      Info  -> ANSI.Green
      Debug -> ANSI.Blue
      Error -> ANSI.Red
      Warn  -> ANSI.Yellow
