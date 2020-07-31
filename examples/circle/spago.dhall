{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "svg-example"
, dependencies =
    [ "effect", "console", "psci-support", "halogen-svg" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
