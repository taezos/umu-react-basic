{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-react-basic-project"
, dependencies =
    [ "console"
    , "effect"
    , "exceptions"
    , "psci-support"
    , "react-basic"
    , "react-basic-hooks"
    , "web-dom"
    , "web-html"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
