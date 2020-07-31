{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "halogen-svg-example"
, dependencies = (../../spago.dhall).dependencies
, packages = (../../spago.dhall).packages
, sources = (../../spago.dhall).sources # ["examples/circle/**/*.purs"]
}
