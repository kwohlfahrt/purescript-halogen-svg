{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "halogen-svg"
, dependencies =
    [ "console"
    , "effect"
    , "halogen"
    , "prelude"
    , "psci-support"
    , "strings"
    , "web-uievents"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
