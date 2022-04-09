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
        , testSystem
            "Parametric basipetal propagation with guard"
            "Fb(1) [ Fb(2) ] Fb(1)" $
          lsystem $ do
            n 1
            axiom "Fa(0) [ Fb(2) ] Fb(1)"
            productions [("Fa(x)" |> "Fb(y)" |: "y < 2", "Fb(y)")]
        ]
    , testGroup
        "Parametric"
        [ testSystem "Basic substitution" "G(1, 2)" $
          lsystem $ do
            n 1
            axiom "F(1, 2)"
            productions [("F(x, y)", "G(x, y)")]
        , testSystem "Addition" "F(3)" $
          lsystem $ do
            n 2
            axiom "F(1)"
            productions [("F(x)", "F(x+1)")]
        , testSystem "Arithmetic" "F(4)" $
          lsystem $ do
            n 1
            axiom "F(1)"
            productions [("F(x)", "F((x*(x+9)/5)^2)")]
        , testSystem "Defines" "F(4)" $
          lsystem $ do
            n 2
            axiom "F(1)"
            define [("y", "x*2")]
            productions [("F(x)", "F(y)")]
        , testSystem "Pre-conditions" "a a b(2)" $
          lsystem $ do
            n 2
            axiom "b(2) a a"
            productions [("b", "a"), ("b(x)" <| "a", "b(x)")]
        , testSystem "Post-conditions" "b(2) a a" $
          lsystem $ do
            n 2
            axiom "a a b(2)"
            productions [("b", "a"), ("a" |> "b(x)", "b(x)")]
        , testSystem "Guards" "b a(2) c(3) d(4)" $
          lsystem $ do
            n 2
            axiom "a(1) a(2) a(3) a(4)"
            productions
              [ ("a(x)" |: "x < 2", "b")
              , ("a(x)" |: "x = 3", "c(x)")
              , ("a(x)" |: "x >= 4", "d(x)")
              ]
        , testSystem "Guards with defines" "b a(2) c(3)" $
          lsystem $ do
            n 2
            axiom "a(1) a(2) a(3)"
            define [("y", "3")]
            productions
              [("a(x)" |: "x <= 1", "b"), ("a(x)" |: "x + y > 5", "c(x)")]
        , testSystem "Guards take precedence" "b c(2)" $
          lsystem $ do
            n 2
            axiom "a(1) a(2)"
            productions [("a(x)", "c(x)"), ("a(x)" |: "x <= 1", "b")]
        , testSystem "Recursive substitution" "!(4) !(2) !(1) A" $
          lsystem $ do
            n 3
            axiom "A"
            productions [("A", "!(1) A"), ("!(w)", "!(w*2)")]
        ]
    ]
