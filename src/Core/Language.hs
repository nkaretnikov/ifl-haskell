module Core.Language where

data Expr a = EVar String            -- variables
            | ENum Int               -- numbers
            | EConstr Int Int        -- constructor tag arity
            | EAp (Expr a) (Expr a)  -- applications
            | ELet                   -- let(rec) expression
                IsRec                  -- recursive? (boolean)
                [(a, Expr a)]          -- definitions
                (Expr a)               -- body of let(rec)
            | ECase                  -- case expression
                (Expr a)               -- expression to scrutinize
                [Alter a]              -- alternatives
            | ELam [a] (Expr a)      -- lambda abstractions
            deriving (Show, Eq)
type Name     = String
type CoreExpr = Expr Name

type IsRec = Bool
recursive, nonRecursive :: IsRec
recursive    = True
nonRecursive = False

bindersOf :: [(a,b)] -> [a]
bindersOf defns = [name | (name, _) <- defns]

rhssOf :: [(a,b)] -> [b]
rhssOf defns = [rhs | (_, rhs) <- defns]

type Alter a = ( Int            -- tag
               , [a]            -- list of bound variables
               , Expr a         -- expression to the right of the arrow
               )

type CoreAlt = Alter Name

-- | Return 'True' if passed to an expression with no internal
-- structure.
isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar _) = True
isAtomicExpr (ENum _) = True
isAtomicExpr _        = False

-- | A Core language program is just a list of supercombinator
-- defintions.
type Program a = [ScDefn a]
type CoreProgram = Program Name

-- | Supercombinator definition.
type ScDefn a = ( Name          -- name of the supercombinator
                , [a]           -- its arguments
                , Expr a        -- its body
                )
type CoreScDefn = ScDefn Name

-- | Standard prelude.
{-
I x           = x;
K x y         = x;
K1 x y        = y;
S f g x       = f x (g x);
compose f g x = f (g x);
twice f       = compose f f
-}
preludeDefs :: CoreProgram
preludeDefs =
  [ ("I", ["x"], EVar "x")
  , ("K", ["x","y"], EVar "x")
  , ("K1", ["x", "y"], EVar "y")
  , ("S", ["f","g","x"], EAp (EAp (EVar "f") (EVar "x"))
                             (EAp (EVar "g") (EVar "x")))
  , ("compose", ["f","g","x"], EAp (EVar "f")
                                   (EAp (EVar "g") (EVar "x")))
  , ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f"))
                         (EVar "f"))
  ]
