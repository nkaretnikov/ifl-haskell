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

data ParseError = ParseError { errorLine  :: Int
                             , unexpected :: String
                             , expected   :: String
                             } deriving Eq

instance Show ParseError where
  show e = "line " ++ show (errorLine e) ++ ":" ++
           " unexpected " ++ unexpected e ++
           "; expected " ++ expected e

type Parser a = [Token] -> (Either ParseError (a, [Token]))

pLit :: String -> Parser String
pLit x []     = Left $ ParseError 1 "empty input" (show x)
pLit x ((n,s):ts)
  | x == s    = Right (s,ts)
  | otherwise = Left $ ParseError n (show s) (show x)

pVar :: Parser String
pVar []       = Left $ ParseError 1 "empty input" "a variable"
pVar ((n,s):ts)
  -- If the first character is a letter, the token is a variable (see
  -- the 'isAlpha c' case in the definition of 'clex').
  -- Since 'Token' is never empty, it is safe to use 'head' here.
  | isAlpha $ head s = Right (s,ts)
  | otherwise        = Left $ ParseError n (show s) "a variable"

-- | Apply two parsers to the same input.
pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 ts = case p1 ts of
  Right res1 -> Right res1
  Left e1    -> case p2 ts of
    Right res2 -> Right res2
    Left e2    -> Left . ParseError (errorLine e1) (unexpected e1) $
                    (expected e1) ++ " or " ++ (expected e2)

pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 ts =
  case p1 ts of
    Left e1 -> Left e1
    Right (v1, ts1) -> case p2 ts1 of
      Left e2 -> Left e2
      Right (v2, ts2) -> Right (combine v1 v2, ts2)

pThen3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
pThen3 combine p1 p2 p3 ts =
  case p1 ts of
    Left e1 -> Left e1
    Right (v1, ts1) -> case p2 ts1 of
      Left e2 -> Left e2
      Right (v2, ts2) -> case p3 ts2 of
        Left e3 -> Left e3
        Right (v3, ts3) -> Right (combine v1 v2 v3, ts3)

pThen4 :: (a -> b -> c -> d -> e)
       -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
pThen4 combine p1 p2 p3 p4 ts =
  case p1 ts of
    Left e1 -> Left e1
    Right (v1, ts1) -> case p2 ts1 of
      Left e2 -> Left e2
      Right (v2, ts2) -> case p3 ts2 of
        Left e3 -> Left e3
        Right (v3, ts3) -> case p4 ts3 of
          Left e4 -> Left e4
          Right (v4, ts4) -> Right (combine v1 v2 v3 v4, ts4)

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = (pOneOrMore p) `pAlt` (pEmpty [])

pEmpty :: a -> Parser a
pEmpty x ts = Right (x,ts)

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = pThen (:) p $ pZeroOrMore p