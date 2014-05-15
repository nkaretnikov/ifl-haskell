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

type Token = String             -- 'Token' is never empty

clex :: String -> [Token]
clex [] = []
clex ('|':'|':cs)
  = clex . dropChar '\n' $ dropWhile (/='\n') cs  -- ignore comments
    where
      dropChar c []     = []
      dropChar c (x:xs) = if c == x then xs else (x:xs)
clex (c1:c2:cs)
  | isOperator [c1,c2] = [c1,c2] : clex cs
    where
      twoCharOps   = ["==", "~=", ">=", "<=", "->"]
      isOperator o = o `elem` twoCharOps
clex (c:cs)
  | isSpace c = clex cs
  | isDigit c = let numToken = c : takeWhile isDigit cs
                    rest     = dropWhile isDigit cs
                in numToken : clex rest
  | isAlpha c = let isIdChar c = isAlpha c || isDigit c || (c == '_')
                    varToken   = c : takeWhile isIdChar cs
                    rest       = dropWhile isIdChar cs
                in varToken : clex rest
  | otherwise = [c] : clex cs
    where isSpace c = c `elem` " \t\n"

syntax :: [Token] -> CoreProgram
syntax = undefined

parse :: String -> CoreProgram
parse = syntax . clex
