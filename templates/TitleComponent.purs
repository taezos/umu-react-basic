module Component.Title
  ( mkTitle
  ) where

import Prelude ( ($), pure )
import React.Basic.DOM as R
import React.Basic.Hooks ( ReactComponent, component )
import Effect ( Effect )

type Props = { text :: String }

mkTitle :: Effect ( ReactComponent Props )
mkTitle = do
  component "Title" $ \props -> React.do
    pure $
      R.h1_
        [ R.text props.text ]
