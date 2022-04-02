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
        ]
    ]
    -- TODO: Verify that context-sensitive matches over context free (p. 30)
