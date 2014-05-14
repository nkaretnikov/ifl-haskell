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

module Core.PPrinter where

import Core.Language
import Data.List (intercalate)
import Data.Char (isSpace)

-- | An abstract datatype for pretty-printing.
data Iseq = INil
          | IStr String
          | IAppend Iseq Iseq
          | IIndent Iseq
          | INewline
          deriving Show

-- | Empty 'Iseq'.
iNil :: Iseq
iNil = INil

-- | Turn a 'String' into an 'Iseq'.
iStr :: String -> Iseq
iStr = IStr

-- | Append two 'Iseqs'.
iAppend :: Iseq -> Iseq -> Iseq
iAppend = IAppend

-- | Newline.
iNewline :: Iseq
iNewline = INewline

-- | Indent an 'Iseq'.
iIndent  :: Iseq -> Iseq
iIndent = IIndent

-- | Turn an 'Iseq' into a 'String'.
iDisplay :: Iseq -> String
iDisplay x = flatten 0 [(x, 0)]

flatten :: Int                  -- current column; zero-indexed
        -> [(Iseq, Int)]        -- work list
        -> String               -- result
flatten _ [] = ""
flatten col ((INil, indent) : xs)
  = flatten col xs
flatten col ((IStr x, indent) : xs)
  = x ++ (flatten (col + length spaces) xs)
    where
      spaces = takeWhile isSpace x
flatten col ((IAppend x1 x2, indent) : xs)
  = flatten col ((x1, indent) : (x2, indent) : xs)
flatten col ((INewline, indent) : xs)
  = '\n' : (space indent) ++ (flatten indent xs)
    where
      space n = replicate n ' '
flatten col ((IIndent x, indent) : xs)
  = flatten col ((x, col) : xs)


-- Pretty-printing.

iConcat :: [Iseq] -> Iseq
iConcat = foldl iAppend iNil

iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave _   []     = iNil
iInterleave sep (x:xs) = x `iAppend` (iConcat $ map (iAppend sep) xs)

pprAExpr e
  | isAtomicExpr e = pprExpr e
  | otherwise      = (iStr "(") `iAppend` (pprExpr e) `iAppend` (iStr ")")

-- | Pretty-print an expression.
pprExpr :: CoreExpr -> Iseq
pprExpr (EVar v) = iStr v
pprExpr (ENum n) = iStr $ show n
pprExpr (EAp (EAp (EVar o) e1) e2)
  | any (== o) ["*", "/", "+", "-", "==", "~=", ">", ">=", "<", "<=", "&", "|"]
    = iConcat [pprAExpr e1, iStr " ", iStr o, iStr " ", pprAExpr e2]
pprExpr (EAp e1 e2)
  = (pprExpr e1) `iAppend` (iStr " ") `iAppend` (pprAExpr e2)
pprExpr (ELet isrec defns expr)
  = iConcat [ iStr keyword, iNewline
            , iStr " ", iIndent (mapSep pprDefn defns), iNewline
            , iStr "in ", pprExpr expr
            ]
    where
      keyword | isrec     = "letrec"
              | otherwise = "let"

      pprDefn :: (Name, CoreExpr) -> Iseq
      pprDefn (name, expr)
        = iConcat [iStr name, iStr " = ", pprExpr expr]

pprExpr (ECase expr alts)
  = iConcat [ iStr "case ", pprExpr expr, iStr " of"
            , iNewline, iStr " ", iIndent (mapSep pprAlt alts)
            ]
  where
    pprAlt :: Alter Name -> Iseq
    pprAlt (tag, vars, expr) =
      iConcat [ iStr $ show tag
              , iStr $ concatMap ((:) ' ') vars
              , iStr " -> ", pprExpr expr
              ]
pprExpr (ELam vars expr)
  = iConcat [ iStr "\\", iStr $ intercalate " " vars
            , iStr " ->", iNewline, iStr " "
            , iIndent (pprExpr expr)
            ]

-- Helper.
mapSep :: (a -> Iseq) -> [a] -> Iseq
mapSep f xs = iInterleave sep (map f xs)
  where
    sep = iConcat [iStr ";", iNewline]

pprint prog = iDisplay (pprProgram prog)

pprProgram :: CoreProgram -> Iseq
pprProgram scdefs =
  iConcat $ map (\scdef -> pprScDefn scdef `iAppend` iNewline) scdefs
    where
      pprScDefn (name, args, expr) =
        iConcat [ iStr name, iStr $ concatMap ((:) ' ') args
                , iStr " = ", pprExpr expr
                ]
