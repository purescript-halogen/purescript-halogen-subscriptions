{ name = "halogen-subscriptions"
, license = "Apache-2.0"
, repository = "https://github.com/purescript-halogen/purescript-halogen-subscriptions"
, dependencies =
  [ "arrays"
  , "effect"
  , "foldable-traversable"
  , "functors"
  , "refs"
  , "safe-coerce"
  , "unsafe-reference"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
