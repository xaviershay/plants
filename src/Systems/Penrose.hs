{-# LANGUAGE OverloadedStrings #-}

module Systems.Penrose where

import Plants.LSystem
import Plants.Prelude

import Data.String (IsString(..), fromString)

-- Penrose tiling using the "classic" L-System method. This works well for
-- stenciling the tiling, but is tricky to color since the tiles are all
-- drawn interspersed with one another.
penroseStencil =
  lsystem $ do
    n 4
    theta 36
    axiom (singleCharWord "[N]++[N]++[N]++[N]++[N]")
    productions $
      [ ("M", singleCharWord "OF++PF----NF[-OF----MF]++")
      , ("N", singleCharWord "+OF--PF[---MF--NF]+")
      , ("O", singleCharWord "-MF++NF[+++OF++PF]-")
      , ("P", singleCharWord "--OF++++MF[+PF++++NF]--NF")
      , ("F", "")
      ]

-- An alternate method based on the sub-divisions described at
-- https://preshing.com/20110831/penrose-tiling-explained/
--
-- For ease of coloring and comprehension, the method is split into two
-- "parts". The first part are the symbols {dL, dR, rL, rR} which map exactly
-- to the two pairs of left/right triangles needed for the method, and which
-- always produce a sub-division of themselves per instructions.
--
-- They always paired with turtle instruction to draw the whole shape (rather
-- than just the half they are tracking), but these instructions produce
-- nothing - in the other words they always "dissappear" and are replaced in
-- the next step by fresh instructions. This does result in some doubling up
-- along the edges, but this doesn't interfere with how they are currently
-- rendered.
--
-- While not strictly necessary, for cleanliness {<, >} symbols are added
-- such that all {+, -} symbols between them can be removed each step.
-- (Without this they would simply hang around redundantly at the end of the
-- word.) This isn't needed for {F} because that symbol is only ever used
-- inside of {<, >}
diamondL = "dL(x * gr) < {(1) F(x) + F(x) + + + + F(x) + F(x) } >"

diamondR = "dR(x * gr) < {(1) F(x) - F(x) - - - - F(x) - F(x) } >"

rhombusL = "rL(x * gr) < {(2) F(x) - - F(x) - - - F(x) - - F(x) } >"

rhombusR = "rR(x * gr) < {(2) F(x) + + F(x) + + + F(x) + + F(x) } >"

penrose =
  lsystem $ do
    n 5
    theta 36
    axiom
      (fromString . intercalate " " . replicate 5 $ "[ dL(1) ] [ dR(1) ] + + ")
    ignore "F + -"
    define
      [ ("gr", showFullPrecision (2 / (1 + sqrt (5)))) -- Golden Ratio
      ]
    productions
      [ ("a(x)", rhombusL)
      -- For each of these four production rules: move the turtle to a
      -- new corner, then draw the required sub-divisions at a smaller
      -- scale.
      , ("dL(x)", "+ f(x / gr) | + [ " <> rhombusL <> " ] " <> diamondL)
      , ( "rL(x)"
        , "- f(x / gr / gr) | [ " <> rhombusL <> " ] f(x) - [ " <> diamondR <>
          " ] " <>
          rhombusR)
      , ("dR(x)", "- f(x / gr) | - [ " <> rhombusR <> " ] " <> diamondR)
      , ( "rR(x)"
        , "+ f(x / gr / gr) | [ " <> rhombusR <> " ] f(x) + [ " <> diamondL <>
          " ] " <>
          rhombusL)
      , ("<", "")
      , (">", "")
      , ("F", "")
      , ("<" <| "+" |> ">", "")
      , ("<" <| "-" |> ">", "")
      ]

singleCharWord :: IsString a => String -> a
singleCharWord = fromString . intercalate " " . fmap pure
