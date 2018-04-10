module Core.PPrinter.Tests (tests) where

import Test.HUnit (Test(..), (~=?))
import Core.Language
import Core.PPrinter

tests = TestList $
  [ TestLabel "Exercise 1.6: 'ELet' indentation" $
      "let\n x = 42\nin x" ~=?
      (ppr $ ELet nonRecursive [("x", ENum 42)] (EVar "x"))
  , TestLabel "'ELet': letrec" $
      "letrec\n x = 42\nin x" ~=?
      (ppr $ ELet recursive [("x", ENum 42)] (EVar "x"))
  , TestLabel "'ELet': two bindings" $
      "let\n x = 42;\n y = 43\nin x" ~=?
      (ppr $ ELet nonRecursive [("x", ENum 42), ("y", ENum 43)] (EVar "x"))
  , TestLabel "'ELet': nested binding" $
      "let\n x = let\n  x' = 42\n in x';\n y = 43\nin x" ~=?
      (ppr $ ELet nonRecursive
       [ ("x", (ELet nonRecursive [("x'", ENum 42)] (EVar "x'")))
       , ("y", ENum 43)
       ] (EVar "x"))
  , TestLabel "'ELet': nested expression" $
      -- XXX: shouldn't it return "let\n x = 42\nin let\n    y = 43\n   in y"?
      "let\n x = 42\nin let\n y = 43\nin y" ~=?
      (ppr $ ELet nonRecursive [("x", ENum 42)]
       (ELet nonRecursive [("y", ENum 43)] (EVar "y")))
  , TestLabel "'ECase': trivial" $
      "case 42 of\n 1 x -> x" ~=?
      (ppr $ ECase (ENum 42) [(1, ["x"], EVar "x")])
  , TestLabel "'ECase': no variables" $
      "case 42 of\n 1 -> 43" ~=?
      (ppr $ ECase (ENum 42) [(1, [], ENum 43)])
  , TestLabel "'ECase': two cases" $
      "case 42 of\n 1 x y -> y;\n 1 x -> x" ~=?
      (ppr $ ECase (ENum 42) [(1, ["x","y"], EVar "y"), (1, ["x"], EVar "x")])
  , TestLabel "'ECase': nested case" $
      "case 42 of\n 1 x y -> case y of\n  1 -> 43;\n 1 x -> x" ~=?
      (ppr $ ECase (ENum 42)
       [ (1, ["x","y"], ECase (EVar "y") [(1, [], ENum 43)])
       , (1, ["x"], EVar "x")
       ])
  , TestLabel "Precedence: parentheses" $
      "(x + y) * z" ~=?
      (ppr $ EAp (EAp (EVar "*")
                      (EAp (EAp (EVar "+")
                                (EVar "x"))
                           (EVar "y")))
                 (EVar "z"))
  , TestLabel "Precedence: no parentheses" $
      "x + y * z" ~=?
      (ppr $ EAp (EAp (EVar "+")
                      (EVar "x"))
                 (EAp (EAp (EVar "*")
                           (EVar "y"))
                      (EVar "z")))
  , TestLabel "Precedence: prefix function application" $
      "x + f y z" ~=?
      (ppr $ EAp (EAp (EVar "+")
                      (EVar "x"))
                 (EAp (EAp (EVar "f")
                           (EVar "y"))
                      (EVar "z")))
  , TestLabel "Exercise 1.8: Infix operator application" $
      "x + y > p * length xs" ~=?
      (ppr $ EAp (EAp (EVar ">")
                      (EAp (EAp (EVar "+")
                                (EVar "x"))
                           (EVar "y")))
                 (EAp (EAp (EVar "*")
                           (EVar "p"))
                      (EAp (EVar "length")
                           (EVar "xs"))))
  , TestLabel "'ELam' indentation" $
      "\\x y z .\n x" ~=?
      (ppr $ ELam ["x","y","z"] (EVar "x"))
  ]
    where
      ppr = iDisplay . flip pprExpr 0
