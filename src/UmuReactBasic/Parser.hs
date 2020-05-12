module UmuReactBasic.Parser where

import qualified Data.Text             as T
import           Data.Version          (showVersion)
import           Import
import           Options.Applicative
import           Paths_umu_react_basic

data Command
  = CommandInit ( Maybe Text )
  | CommandComponent Text Text
  deriving ( Show )

parseCommand :: Parser Command
parseCommand = subparser $
  ( command "init" $ parseCommandInit `withInfo` "Initialize scaffold" )
  <>
  ( command "component" $ parseCommandComponent `withInfo` "Generate component" )

parseCommandInit :: Parser Command
parseCommandInit = CommandInit <$> initParser
  where
    initParser :: Parser ( Maybe Text )
    initParser = optional $
      argument str ( metavar "LOCATION" <> help "Location to generate scaffold" )

parseCommandComponent :: Parser Command
parseCommandComponent = CommandComponent <$> pathParser <*> nameParser
  where
    pathParser :: Parser Text
    pathParser = argument str
      ( metavar "[COMPONENT_PATH]" <> help "Location to generate component" )

    nameParser :: Parser Text
    nameParser = argument str
      ( metavar "[COMPONENT_NAME]" <> help "Name of the component to generate" )

parseVersion :: Parser ( a -> a )
parseVersion = infoOption ( concat [ ( T.unpack appName ) <> " " <> showVersion version ] )
  ( short 'v' <> long "version" <> help "Show version" <> hidden )

withInfo :: Parser Command -> String -> ParserInfo Command
withInfo opts desc = info ( helper <*> opts ) $ progDesc desc
