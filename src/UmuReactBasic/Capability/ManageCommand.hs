module UmuReactBasic.Capability.ManageCommand
  ( ManageCommand (..)
  , generateProj
  ) where

import           Import
import qualified Turtle
import qualified Turtle.Prelude     as TP
import           UmuReactBasic.Util

class Monad m => ManageCommand m where
  generateProject :: Maybe Text -> m ()

instance ManageCommand IO where
  generateProject = liftIO . generateProject

-- ManageCommand constraint is just so this function can only be used if
-- there's an instance of ManageCommand
generateProj
  :: ( MonadIO m, ManageCommand m )
  => Maybe Text
  -> m ()
generateProj mLoc = do
  writeSrc mLoc

writeSrc :: MonadIO m => Maybe Text -> m ()
writeSrc mLoc = do
  liftIO $ TP.mkdir ( Turtle.fromText $ mkPathName mLoc "src" )
  mkMessage "Generating src..."
