{ name = "halogen-subscriptions"
, dependencies =
  [ "arrays"
  , "effect"
  , "filterable"
  , "foldable-traversable"
  , "functors"
  , "refs"
  , "safe-coerce"
  , "unsafe-reference"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
