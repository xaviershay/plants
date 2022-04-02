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
        [ testSystem "Trivial (p. 4)" "a b a b a a b a" $
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
    ]
