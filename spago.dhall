{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "arrays"
    , "assert"
    , "console"
    , "effect"
    , "ordered-collections"
    , "psci-support"
    , "tuples"
    , "uuid"
    , "string-rewriting"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
