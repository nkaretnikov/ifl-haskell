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

module Core.Parser where

import Core.Language
import Data.Char (isDigit, isAlpha)

type LineNumber = Int
type Token = (LineNumber, String)  -- 'Token' is never empty

clex :: String -> LineNumber -> [Token]
clex [] _ = []
clex ('|':'|':cs) n
  = clex (dropChar '\n' $ dropWhile (/='\n') cs) (n + 1)  -- ignore comments
    where
      dropChar c []     = []
      dropChar c (x:xs) = if c == x then xs else (x:xs)
clex (c1:c2:cs) n
  | isOperator [c1,c2] = (n, [c1,c2]) : clex cs n
    where
      twoCharOps   = ["==", "~=", ">=", "<=", "->"]
      isOperator o = o `elem` twoCharOps
clex (c:cs) n
  | isNewline c = clex cs (n + 1)
  | isSpace c   = clex cs n
  | isDigit c = let numToken = c : takeWhile isDigit cs
                    rest     = dropWhile isDigit cs
                in (n, numToken) : clex rest n
  | isAlpha c = let isIdChar c = isAlpha c || isDigit c || (c == '_')
                    varToken   = c : takeWhile isIdChar cs
                    rest       = dropWhile isIdChar cs
                in (n, varToken) : clex rest n
  | otherwise = (n, [c]) : clex cs n
    where
      isNewline c = c == '\n'
      isSpace c   = c `elem` " \t"

syntax :: [Token] -> CoreProgram
syntax = undefined

parse :: String -> CoreProgram
parse = syntax . flip clex 1
