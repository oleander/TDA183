module Eval1 where
import IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import Parchi
import Lexchi
import Skelchi
import Printchi
import Abschi
import ErrM
import Data.List
import Debug.Trace

main = do
  test1
  test2

-- Tests pp, evaltot and eval
test1 = do 
    assert (decodenat exp == (n1 + n2)) "should evaluate n1 + n2"
    putStrLn ("Evaluated (add n1 n2) to: " ++ (pp exp))
    putStrLn ("Add is defined as: " ++ (pp add2))
    where n1 = 3
          n2 = 2
          e1 = codenat n1
          e2 = codenat n2
          exp = evaltot (Apply add2 [e1, e2])
          add2 = parse (pp add) -- Call parse multiply times to verify that pp works

test2 = do
  assert ((decodenat (evaltot l)) == (n + 1)) "should increment using def"
  putStrLn ("The calculation is defined as: " ++ (pp l))
  where
    n = 1
    d1 = Def (LIdent "a") (codenat n)
    exp = parse (pp (parse "S a")) -- Test pp
    l = (Let [d1] exp)

assert :: Bool -> String -> IO()
assert False message = error ("Failed: " ++ message)
assert True message  = putStrLn ("Success: " ++ message)

substs :: Exp -> [LIdent] -> [Exp] -> Exp
substs exp [] []         = exp
substs exp (i:is) (e:es) = substs (subst exp i e) is es
substs exp _      _      = error "wrong number of argumnets"

subst :: Exp -> LIdent -> Exp -> Exp
subst (Apply exp1 exp2s) l e 
  = Apply (subst exp1 l e) (map (\exp -> subst exp l e) exp2s)

subst (Rec value1 exp) value2 e
  | value1 == value2 = Rec value1 exp
  | otherwise        = Rec value1 (subst exp value2 e)

-- Replace {ident} with {e} if {ident} == {l}
subst (Var ident) l e = replaceLIdent ident l e

-- Do not replace constant values
subst (Const x) l e = Const x

-- Replace all occurrences of {l} in {exp} with {e}, if equal
subst (Case exp branches) l e 
  = Case (subst exp l e) (replaceBranch branches l e)

subst (Lambda idents exp) l e
  -- Do not replace if {l} is already bound in {idents}
  | elem l idents = Lambda idents exp
  -- Replace if {l} is not bound in {idents}
  | otherwise      = Lambda idents (subst exp l e)
subst _ _ _ = error "not defined for vars"

-- Helper functions
-- Replaces occurrences in a list of branches
replaceBranch :: [Branch] -> LIdent -> Exp -> [Branch]
replaceBranch [] l e                    = []
replaceBranch ((Branch ident exp):branches) l e 
  = (Branch ident (subst exp l e)):(replaceBranch branches l e)

-- Replaces {ll} with {e} if {ll} == {l}
-- Otherwise just return {ll}
replaceLIdent :: LIdent -> LIdent -> Exp -> Exp
replaceLIdent ll l e
  | ll == l   = e
  | otherwise = Var ll

lookupBr :: UIdent -> [Branch] -> Maybe Exp
lookupBr ident1 []   = Nothing
lookupBr ident1 ((Branch ident2 exp):branches)
  | ident1 == ident2 = Just exp
  | otherwise        = lookupBr ident1 branches

-- Start: prune
prune :: Exp -> Exp
prune (Let xs exp) = Apply lambda vars
  where args   = justLIdent xs
        vars   = justExp xs
        lambda = Lambda args exp
prune   _      = error "invalid args"

justLIdent :: [Def] -> [LIdent]
justLIdent []               = []
justLIdent ((Def l _):xs)   = l : (justLIdent xs)

justExp :: [Def] -> [Exp]
justExp []               = []
justExp ((Def _ exp):xs) = exp : (justExp xs)
-- End: prune

codenat :: Int -> Exp
codenat 0     = (Apply (Const (UIdent "Z")) [])
codenat n
  | n < 0     = error "not defined for negative values"
  | otherwise = (Apply (Const (UIdent "S")) [(codenat (n - 1))])

decodenat :: Exp -> Int
decodenat (Apply (Const (UIdent "Z")) [])     = 0
decodenat (Apply (Const (UIdent "S")) (x:[])) = 1 + (decodenat x)
decodenat _                                   = error "undefined for given argumnets"

add = parse "rec add = \\x y. case x of { Z -> y; S -> \\n. S (add n y)}"

-------------------------------------
printExp e = putStr ((printTree e)++"\n")
parse :: String -> Exp
parse s = case (pExp (myLexer s)) of
           Bad s2   -> error ("\nParse Failed...\n   " ++ s2)
           Ok  tree -> tree 
parsef file = do
        pString <- (readFile file) 
        printExp (parse pString)

eval :: Exp -> Exp 
eval (Case e t)  =
  case (eval e) of
    (Apply (Const i) es) ->
      case (lookupBr i t) of
        Just e'  -> eval (Apply e' es)
        Nothing  -> error "no match"
    _                    -> error "no match"
eval (Const i)                               = Apply (Const i) []
eval (Var i)                                 = Var i
eval (Apply (Const i) es)                    = Apply (Const i) es
eval (Apply e es) =
  case (eval e) of
    (Apply (Const i) ds) -> Apply (Const i) (ds ++ es)
    (Lambda vars exp)    -> eval (substs exp vars es)
    e                    -> error "not valid"
eval (Lambda var exp)                        = Lambda var exp
eval (Rec ident exp)                         = eval (subst exp ident (Rec ident exp))
-- I know there's no inductive definition of the abstract syntax for let's
-- but it would be nice to have one â€“ this code does the trick
eval (Let defs exp)                          = eval (prune (Let defs exp))

evaltot :: Exp -> Exp
evaltot (Apply (Const i) exps) = Apply (Const i) (map evaltot exps)
evaltot (Lambda vars exp)      = Lambda vars exp
evaltot (Var i)                = Var i
evaltot e                      = evaltot (eval e)

pp :: Exp -> String
pp exp = printTree exp

ppDef :: Def -> String
ppDef (Def (LIdent var) exp) = var ++ " = " ++ (pp exp)

filterBranch :: Branch -> String
filterBranch (Branch (UIdent value) exp) = value ++ " -> " ++ (pp exp)

filterIdent :: [LIdent] -> [String]
filterIdent [] = []
filterIdent ((LIdent v):xs) = v : (filterIdent xs)