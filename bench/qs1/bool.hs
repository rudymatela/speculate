import Test.QuickSpec

main =
  quickSpec
    [ ["p", "q", "r"] `vars` (undefined :: Bool)
    , "False" `fun0` False
    , "True"  `fun0` True
    , "not"   `fun1` not
    , "&&"    `fun2` (&&)
    , "||"    `fun2` (||)
    ]
