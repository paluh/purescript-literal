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
    , "integers"
    , "numbers"
    , "partial"
    , "psci-support"
    , "unsafe-coerce"
    , "typelevel-prelude"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
