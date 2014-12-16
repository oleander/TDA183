module Eval2 where
import Data.List
import Debug.Trace

newtype UIdent = UIdent String deriving (Eq,Ord,Show,Read)
newtype LIdent = LIdent String deriving (Eq,Ord,Show,Read)

data PExp =
   Lambda [LIdent] PExp
 | Primrec PExp PExp PExp
 | Apply PExp [PExp]
 | Var LIdent
 | Const UIdent
  deriving (Eq,Ord,Show,Read)

-- Test
main = do
  assert ((add2 a b) == (a + b)) "should add two numbers"
  where a = 5
        b = 2

assert :: Bool -> String -> IO()
assert False message = error ("Failed: " ++ message)
assert True message  = putStrLn ("Success: " ++ message)

substs :: PExp -> [LIdent] -> [PExp] -> PExp
substs exp [] []         = exp
substs exp (i:is) (e:es) = substs (subst exp i e) is es
substs exp _      _      = error "wrong number of argumnets"

subst :: PExp -> LIdent -> PExp -> PExp
subst (Apply exp1 exp2s) l e 
  = Apply (subst exp1 l e) (map (\exp -> subst exp l e) exp2s)

-- Replace {ident} with {e} if {ident} == {l}
subst (Var ident) l e = replaceLIdent ident l e

-- Do not replace constant values
subst (Const x) l e = Const x

subst (Lambda idents exp) l e
  -- Do not replace if {l} is already bound in {idents}
  | elem l idents = Lambda idents exp
  -- Replace if {l} is not bound in {idents}
  | otherwise      = Lambda idents (subst exp l e)

subst (Primrec e1 e2 e3) l e = Primrec ee1 ee2 ee3
  where 
    ee1 = subst e1 l e
    ee2 = subst e2 l e
    ee3 = subst e3 l e

codenat :: Int -> PExp
codenat 0     = Apply (Const (UIdent "Z")) []
codenat n
  | n < 0     = error "not defined for negative values"
  | otherwise = (Apply (Const (UIdent "S")) [(codenat (n - 1))])

decodenat :: PExp -> Int
decodenat (Apply (Const (UIdent "Z")) [])     = 0
decodenat (Apply (Const (UIdent "S")) (x:[])) = 1 + (decodenat x)
decodenat _                                   = error "undefined for given argumnets"

evalp :: PExp -> PExp
evalp (Const i)                               = Apply (Const i) []
evalp (Var i)                                 = Var i
evalp (Apply (Const i) es)                    = Apply (Const i) es
evalp (Apply e es)                            =
  case evalp e of
    (Apply (Const i) ds) -> Apply (Const i) (ds ++ es)
    (Lambda vars exp)    -> evalp (substs exp vars es)
    e                    -> error "not valid"
evalp (Lambda var exp)                        = Lambda var exp
evalp (Primrec exp g f)                           = 
  case evalp exp of
    Apply (Const (UIdent "Z")) []        -> evalp g
    (Apply (Const (UIdent "S")) (a1:[])) -> evalp (Apply f [a1, a2])
                                            where a2 = Primrec a1 g f
    _                                    -> error "not defined"

evaltotp :: PExp -> PExp
evaltotp (Apply (Const i) exps) = Apply (Const i) (map evaltotp exps)
evaltotp (Lambda vars exp)      = Lambda vars exp
evaltotp (Var i)                = Var i
evaltotp e                      = evaltotp (evalp e)

-- Replaces {ll} with {e} if {ll} == {l}
-- Otherwise just return {ll}
replaceLIdent :: LIdent -> LIdent -> PExp -> PExp
replaceLIdent ll l e
  | ll == l   = e
  | otherwise = Var ll

add2 :: Int -> Int -> Int
add2 a b = decodenat (evaltotp rec)
  where
    exp = Apply (Const (UIdent "S")) [Var (LIdent "y")] -- b + 1
    h   = Lambda [(LIdent "x"), (LIdent "y")] exp
    g   = codenat b
    e   = codenat a
    rec = Primrec e g h