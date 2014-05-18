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
      [(1,"foo"), (2,"bar")] ~=? (clex "foo||a comment\nbar" 1)
  , TestLabel "Exercise 1.10: Recognize two-character operators" $
      [ (1,"=="), (1,"~="), (1,"~"), (1,"~"), (1,">=")
      , (1,"<="), (1,"->"), (1,"<"), (1,"-")
      ] ~=? (clex "== ~= ~~ >= <= -> <-" 1)
  , TestLabel "Exercise 1.11: Attach a line number to each token" $
      [(1,"foo"), (3,"bar")] ~=? (clex "foo \n\n bar" 1)
  , TestLabel "'pLit': empty input" $
      (Left . ParseError 1 "empty input" $ show "foo") ~=? (pLit "foo" [])
  , TestLabel "'pLit': matching string" $
      Right ("foo", [(1,"bar")]) ~=? (pLit "foo" $ [(1,"foo"), (1,"bar")])
  , TestLabel "'pLit': failure to parse" $
      (Left . ParseError 1 (show "hello") $ show "goodbye") ~=?
        (pLit "goodbye" [(1,"hello"), (1,"rest")])
  , TestLabel "'pVar': variable" $
      Right ("foo", [(2,"bar")]) ~=? (pVar [(1,"foo"), (2,"bar")])
  , TestLabel "'pVar': not a variable" $
      (Left $ ParseError 1 (show "1") "a variable") ~=?
        (pVar [(1,"1"), (1,"foo")])
  , TestLabel "'pAlt': first match" $
      Right ("hello", [(1,"rest")]) ~=?
        (pHelloOrGoodbye [(1,"hello"), (1,"rest")])
  , TestLabel "'pAlt': second match" $
      Right ("goodbye", [(1,"rest")]) ~=?
        (pHelloOrGoodbye [(1,"goodbye"), (1,"rest")])
  , TestLabel "'pAlt': parse failure" $
      (Left . ParseError 1 (show "foo") $
       (show "hello") ++ " or " ++ (show "goodbye")) ~=?
         (pHelloOrGoodbye [(1,"foo"), (1,"rest")])
  , TestLabel "'pThen': both parsers succeed" $
      Right (("hello","James"), [(1,"!")]) ~=?
        (pGreeting [(1,"hello"), (1,"James"), (1,"!")])
  , TestLabel "'pThen': first parser fails" $
      (Left . ParseError 1 (show "hi") $
       show "hello" ++ " or " ++ show "goodbye") ~=?
         (pGreeting [(1,"hi"), (1,"James"), (1,"!")])
  , TestLabel "'pThen': second parser fails" $
      (Left $ ParseError 1 (show "42") "a variable") ~=?
        (pGreeting [(1,"hello"), (1,"42"), (1,"!")])
  , TestLabel "'pThen': both parsers fail" $
      (Left . ParseError 1 (show "hi") $
       show "hello" ++ " or " ++ show "goodbye") ~=?
         (pGreeting [(1,"hi"), (1,"42"), (1,"!")])
  ]

pHelloOrGoodbye :: Parser String
pHelloOrGoodbye = (pLit "hello") `pAlt` (pLit "goodbye")

pGreeting :: Parser (String, String)
pGreeting = pThen (,) pHelloOrGoodbye pVar