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

module Core.Parser.Tests (tests) where

import Test.HUnit (Test(..), (~=?))
import Core.Parser

tests = TestList $
  [ TestLabel "Exercise 1.9: Ignore comments" $
      ["foo", "bar"] ~=? (clex "foo||a comment\nbar")
  , TestLabel "Exercise 1.10: Recognize two-character operators" $
      ["==","~=","~","~",">=","<=","->","<","-"]  ~=?
      (clex "== ~= ~~ >= <= -> <-")
  ]