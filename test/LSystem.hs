{-# LANGUAGE OverloadedStrings #-}

module LSystem where

import TestPrelude

import Plants.LSystem

testSystem :: TestName -> MWord ModuleFixed -> LSystem -> TestTree
testSystem name expected system =
  testCase name $ expected @=? (view lsysAxiom (run system))

test_run =
  testGroup
    "LSystem.run"
    [ testGroup
        "Deterministic & Context-free (DOL)"
        [ testSystem "First example (p. 4)" "a b a b a a b a" $
          lsystem $ do
            axiom "b"
            n 5
            productions [("b", "a"), ("a", "b a")]
        , testSystem "Anabaena catenula (p. 5)" "b◀ a▶ b◀ a▶ a▶ b◀ a▶ a▶" $
          lsystem $ do
            n 4
            axiom "a▶"
            productions
              [("a▶", "a◀ b▶"), ("a◀", "b◀ a▶"), ("b▶", "a▶"), ("b◀", "a◀")]
        ]
    , testGroup
        "Context-sensitive"
        [ testSystem "Pre-condition (p. 31)" "a a b" $
          lsystem $ do
            axiom "b a a"
            n 2
            productions [("b" <| "a", "b"), ("b", "a")]
        , testSystem "Post-condition (p. 31)" "b a a" $
          lsystem $ do
            axiom "a a b"
            n 2
            productions [("a" |> "b", "b"), ("b", "a")]
        , testSystem "Ignore (p. 32)" "b a + - a" $
          lsystem $ do
            n 2
            axiom "a a + - b"
            ignore "+ -"
            productions [("a" |> "b", "b"), ("b", "a")]
        , testSystem
            "Acropetal propagation (p. 32)"
            "Fb [ + Fb ] Fb [ - Fa ] Fa" $
          lsystem $ do
            ignore "+ -"
            n 1
            axiom "Fb [ + Fa ] Fa [ - Fa ] Fa"
            productions [("Fb" <| "Fa", "Fb")]
        , testSystem
            "Basipetal propagation (p. 32)"
            "Fa [ + Fa ] Fb [ - Fa ] Fb" $
          lsystem $ do
            ignore "+ -"
            n 1
            axiom "Fa [ + Fa ] Fa [ - Fa ] Fb"
            productions [("Fa" |> "Fb", "Fb")]
        , testSystem "Parametric basipetal propagation" "Fb(1) [ Fa(2) ] Fb(1)" $
          lsystem $ do
            n 1
            axiom "Fa(0) [ Fa(2) ] Fb(1)"
            productions [("Fa(x)" |> "Fb(y)", "Fb(y)")]
        -- , testSystem
        --     "Parametric basipetal propagation with guard"
        --     "Fb(1) [ Fa(2) ] Fb(1)" $
        --   lsystem $ do
        --     n 1
        --     axiom "Fa(0) [ Fb(2) ] Fb(1)"
        --     productions [("Fa(x)" |> "Fb(y)" |: "y < 2", "Fb(y)")]
        ]
    , testGroup
        "Parametric"
        [ testSystem "Basic substitution" "G(1, 2)" $
          lsystem $ do
            n 1
            axiom "F(1, 2)"
            productions [("F(x, y)", "G(x, y)")]
        ]
  --  [ testCase "Basic substitution"
  --    $ (parseUnsafe "G(1, 2)") @=?
  --    (stepN gen 1 (parseUnsafe "F(1, 2)") $ productions
  --      [ (match "F(x, y)", "G(x,y)")
  --      ])
  --  , testCase "Addition"
  --    $ (parseUnsafe "F(3)") @=?
  --    (stepN gen 2 (parseUnsafe "F(1)") $ productions
  --      [ (match "F(x)", "F(x+1)")
  --      ])
  --  , testCase "Arithmetic"
  --    $ (parseUnsafe "F(4)") @=?
  --    (stepN gen 1 (parseUnsafe "F(1)") $ productions
  --      [ (match "F(x)", "F((x*(x+9)/5)^2)")
  --      ])
  --  , testCase "Defines"
  --    $ (parseUnsafe "F(4)") @=?
  --    (stepN gen 2 (parseUnsafe "F(1)") $ withDefines [("y", "x*2")] $ productions
  --      [ (match "F(x)", "F(y)")
  --      ])
  --  , testCase "Pre-conditions"
  --    $ (parseUnsafe "a a b(1)") @=?
  --    (stepN gen 2 (parseUnsafe "b(1) a a")
  --    $ productions
  --        [ ("b(x)" <| match "a", "b(x)")
  --        , (match "b", "a")
  --        ])
  --  , testCase "Post-conditions"
  --    $ (parseUnsafe "b(2) a a") @=?
  --    (stepN gen 2 (parseUnsafe "a a b(2)")
  --    $ productions
  --        [ (match "a" |> "b(x)", "b(x)")
  --        , (match "b", "a")
  --        ])
  --  , testCase "Guards"
  --    $ (parseUnsafe "b a(2) c(3) d(4)") @=?
  --    (stepN gen 1 (parseUnsafe "a(1) a(2) a(3) a(4)")
  --    $ productions
  --        [ (match "a(x)" |: "x < 2", "b")
  --        , (match "a(x)" |: "x = 3", "c(x)")
  --        , (match "a(x)" |: "x >= 4", "d(x)")
  --        ])
  --  , testCase "Guards with defines"
  --    $ (parseUnsafe "b a(2) c(3)") @=?
  --    (stepN gen 1 (parseUnsafe "a(1) a(2) a(3)")
  --    $ withDefines [("y", "3")]
  --    $ productions
  --        [ (match "a(x)" |: "x <= 1", "b")
  --        , (match "a(x)" |: "x + y > 5", "c(x)")
  --        ])
  --  , testCase "Guard takes precedence"
  --    $ (parseUnsafe "b c(2)") @=?
  --    (stepN gen 1 (parseUnsafe "a(1) a(2)")
  --    $ productions
  --        [ (match "a(x)", "c(x)")
  --        , (match "a(x)" |: "x <= 1", "b")
  --        ])
    ]
    -- TODO: Verify that context-sensitive matches over context free (p. 30)
