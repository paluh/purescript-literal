{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "literal"
, dependencies =
    [ "assert"
    , "effect"
    , "console"
    , "foreign"
    , "heterogeneous"
    , "integers"
    , "profunctor-lenses"
    , "numbers"
    , "partial"
    , "psci-support"
    , "record"
    , "unsafe-coerce"
    , "typelevel-eval"
    , "typelevel-prelude"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
