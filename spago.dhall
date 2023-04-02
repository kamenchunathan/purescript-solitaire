{ name = "halogen-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign"
  , "halogen"
  , "integers"
  , "lists"
  , "maybe"
  , "prelude"
  , "routing"
  , "routing-duplex"
  , "strings"
  , "tuples"
  , "typelevel-prelude"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
