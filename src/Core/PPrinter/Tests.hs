{- ifl-haskell: "Implementing Functional Languages: a tutorial" in Haskell.
   Copyright 2014 Nikita Karetnikov <nikita@karetnikov.org>

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module Core.PPrinter.Tests (tests) where

import Test.HUnit (Test(..), (~=?))
import Core.Language
import Core.PPrinter

tests = TestList $
  [ TestLabel "Exercise 1.6: 'ELet' indentation" $
      "let\n x = 42\nin x" ~=?
      (ppr $ ELet nonRecursive [("x", ENum 42)] (EVar "x"))
  , TestLabel "Exercise 1.8: Infix operator application" $
      "(x + y) > (p * (length xs))" ~=?
      (ppr $ EAp (EAp (EVar ">")
                      (EAp (EAp (EVar "+")
                                (EVar "x"))
                           (EVar "y")))
                 (EAp (EAp (EVar "*")
                           (EVar "p"))
                      (EAp (EVar "length")
                           (EVar "xs"))))
  ]
    where
      ppr = iDisplay . pprExpr