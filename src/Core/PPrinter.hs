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

flatten col ((INil, _) : xs)
  = flatten col xs

flatten col ((IStr x, _) : xs)
  = x ++ (flatten (col + length spaces) xs)
    where
      spaces = takeWhile isSpace x

flatten col ((IAppend x1 x2, indent) : xs)
  = flatten col ((x1, indent) : (x2, indent) : xs)

flatten _ ((INewline, indent) : xs)
  = '\n' : (space indent) ++ (flatten indent xs)
    where
      space n = replicate n ' '

flatten col ((IIndent x, _) : xs)
  = flatten col ((x, col) : xs)


-- Pretty-printing.

iConcat :: [Iseq] -> Iseq
iConcat = foldl iAppend iNil  -- XXX: use foldl'?

iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave _   []     = iNil
iInterleave sep (x:xs) = x `iAppend` (iConcat $ map (iAppend sep) xs)

type Precedence = Int

-- | Operator predicate.
isOp :: String -> Bool
isOp o = any (== o) [ "*", "/", "+", "-", "==", "~="
                    , ">", ">=", "<", "<=", "&", "|"
                    ]

-- | Operator precedence.
opsPrec :: [(String, Precedence)]
opsPrec = [ ("*",5), ("+",4), ("==",3), ("~=",3), (">",3)
          , (">=",3), ("<",3), ("<=",3), ("&",2), ("|",1)
          ]

-- | Pretty-print an expression.
pprExpr :: CoreExpr -> Precedence -> Iseq
pprExpr (EVar v) _ = iStr v

pprExpr (ENum n) _ = iStr $ show n

pprExpr (EConstr tag ar) _
  = iStr $ "Pack{" ++ show tag ++ "," ++ show ar ++ "}"

pprExpr (EAp (EAp (EVar o) e1) e2) prec | isOp o
  = let o' = iConcat [iStr " ", iStr o, iStr " "]
    in case lookup o opsPrec
       of Nothing ->
            iConcat [pprExpr e1 0, o', pprExpr e2 0]
          Just prec' ->
            if prec' >= prec
            then iConcat [ pprExpr e1 prec', o', pprExpr e2 prec'
                         ]
            else iConcat [ iStr "("
                         , pprExpr e1 prec', o', pprExpr e2 prec'
                         , iStr ")"
                         ]

pprExpr (EAp e1 e2) prec
  = iConcat [ pprExpr e1 prec
            , iStr " "
            , pprExpr e2 prec
            ]

pprExpr (ELet isrec defns expr) prec
  = iConcat [ iStr keyword, iNewline
            , iStr " "
            , iIndent (mapSep (\defn -> pprDefn defn prec) defns)
            , iNewline
            , iStr "in ", pprExpr expr prec
            ]
    where
      keyword | isrec     = "letrec"
              | otherwise = "let"

pprExpr (ECase expr alts) prec
  = iConcat [ iStr "case ", pprExpr expr prec, iStr " of"
            , iNewline, iStr " "
            , iIndent (mapSep (\alt -> pprAlt alt prec) alts)
            ]

pprExpr (ELam vars expr) prec
  = iConcat [ iStr "\\", iStr $ intercalate " " vars
            , iStr " .", iNewline, iStr " "
            , iIndent (pprExpr expr prec)
            ]

pprDefn :: (Name, CoreExpr) -> Precedence -> Iseq
pprDefn (name, expr) prec
  = iConcat [iStr name, iStr " = ", pprExpr expr prec]

pprAlt :: Alter Name -> Precedence -> Iseq
pprAlt (tag, vars, expr) prec =
  iConcat [ iStr $ show tag
          , iStr $ concatMap ((:) ' ') vars
          , iStr " -> ", pprExpr expr prec
          ]

-- Helper.
mapSep :: (a -> Iseq) -> [a] -> Iseq
mapSep f xs = iInterleave sep (map f xs)
  where
    sep = iConcat [iStr ";", iNewline]

pprProgram :: CoreProgram -> Iseq
pprProgram scdefs =
  iConcat $ map (\scdef -> pprScDefn scdef `iAppend` iNewline) scdefs
    where
      pprScDefn (name, args, expr) =
        iConcat [ iStr name, iStr $ concatMap ((:) ' ') args
                , iStr " = ", pprExpr expr 0
                ]

pprint :: CoreProgram -> String
pprint prog = iDisplay (pprProgram prog)
