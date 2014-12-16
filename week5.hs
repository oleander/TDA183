module Eval where
import IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import Parchi
import Lexchi
import Skelchi
import Printchi
import Abschi
import ErrM

-- Tests
main = do 
  assert ((casenat (codenat 5) (codenat 1) id) == (codenat 4)) "should decrement and return using {id}"
  assert ((casenat (codenat 0) (codenat 5) id) == (codenat 5)) "should just return sec arg"
  assert ((codenat 0) == zero) "should encode zero"
  assert ((codenat 1) == one) "should encode one"
  assert ((decodenat zero) == 0) "should decode zero"
  assert ((decodenat one) == 1) "should decode one"
  assert ((decodenat (codenat 5)) == 5) "decodenat and casenat"
  assert ((subst (Var (LIdent "hello")) (LIdent "hello") (Var (LIdent "noo"))) ==  (Var (LIdent "noo"))) "should replace if found"
  assert ((subst (Var (LIdent "hello")) (LIdent "noo") (Var (LIdent "hello"))) ==  (Var (LIdent "hello"))) "should not replace var"
  assert ((subst (Const (UIdent "hello")) (LIdent "hello") (Const (UIdent "noo"))) ==  (Const (UIdent "hello"))) "should not replace const"
  assert ((subst recExp (LIdent "p") (Var (LIdent "e"))) == toExp) "should replace in rec"
  assert ((subst recExp (LIdent "add") (Var (LIdent "e"))) == recExp) "should not replace in rec if bound"
  assert ((subst l1 (LIdent "p") (Var (LIdent "e"))) == l2) "should replace in lambda if not bound"
  assert ((subst l1 (LIdent "x") (Var (LIdent "e"))) == l1) "should not replace in lambda if bound"
  assert ((subst a1 (LIdent "e") (Var (LIdent "z"))) == a2) "should replace in apply #1"
  assert ((subst a2 (LIdent "p") (Var (LIdent "e"))) == a3) "should replace in apply #2"
  assert ((subst c1 (LIdent "p") (Var (LIdent "o"))) == c3) "should replace head in case" 
  assert ((subst c1 (LIdent "add") (Var (LIdent "iii"))) == c2) "should replace body of case" 
  assert ((subst c1 (LIdent "S") (Var (LIdent "O"))) == c1) "should not replace branch key" 
  assert ((prune let1) == let2) "should replace var"

  where
    recExp = Rec (LIdent "add") (Lambda [LIdent "x",LIdent "y"] (Case (Var (LIdent "u")) [Branch (UIdent "Z") (Var (LIdent "y")),Branch (UIdent "S") (Lambda [LIdent "n"] (Apply (Const (UIdent "S")) [Apply (Var (LIdent "add")) [Var (LIdent "p"),Var (LIdent "y")]]))]))
    toExp  = Rec (LIdent "add") (Lambda [LIdent "x",LIdent "y"] (Case (Var (LIdent "u")) [Branch (UIdent "Z") (Var (LIdent "y")),Branch (UIdent "S") (Lambda [LIdent "n"] (Apply (Const (UIdent "S")) [Apply (Var (LIdent "add")) [Var (LIdent "e"),Var (LIdent "y")]]))]))
    l1     = Lambda [LIdent "x",LIdent "y"] (Case (Var (LIdent "r")) [Branch (UIdent "Z") (Var (LIdent "y")),Branch (UIdent "S") (Lambda [LIdent "n"] (Apply (Const (UIdent "S")) [Apply (Var (LIdent "add")) [Var (LIdent "p"),Var (LIdent "y")]]))])
    l2     = Lambda [LIdent "x",LIdent "y"] (Case (Var (LIdent "r")) [Branch (UIdent "Z") (Var (LIdent "y")),Branch (UIdent "S") (Lambda [LIdent "n"] (Apply (Const (UIdent "S")) [Apply (Var (LIdent "add")) [Var (LIdent "e"),Var (LIdent "y")]]))])
    a1     = Apply (Var (LIdent "p")) [(Var (LIdent "e"))]
    a2     = Apply (Var (LIdent "p")) [(Var (LIdent "z"))]
    a3     = Apply (Var (LIdent "e")) [(Var (LIdent "z"))]
    c1     = Case (Var (LIdent "p")) [Branch (UIdent "Z") (Var (LIdent "y")),Branch (UIdent "S") (Lambda [LIdent "n"] (Apply (Const (UIdent "S")) [Apply (Var (LIdent "add")) [Var (LIdent "n"),Var (LIdent "y")]]))]
    c2     = Case (Var (LIdent "p")) [Branch (UIdent "Z") (Var (LIdent "y")),Branch (UIdent "S") (Lambda [LIdent "n"] (Apply (Const (UIdent "S")) [Apply (Var (LIdent "iii")) [Var (LIdent "n"),Var (LIdent "y")]]))]
    c3     = Case (Var (LIdent "o")) [Branch (UIdent "Z") (Var (LIdent "y")),Branch (UIdent "S") (Lambda [LIdent "n"] (Apply (Const (UIdent "S")) [Apply (Var (LIdent "add")) [Var (LIdent "n"),Var (LIdent "y")]]))]
    c4     = Case (Var (LIdent "o")) [Branch (UIdent "Z") (Var (LIdent "y")),Branch (UIdent "O") (Lambda [LIdent "n"] (Apply (Const (UIdent "S")) [Apply (Var (LIdent "add")) [Var (LIdent "n"),Var (LIdent "y")]]))]
    let1   = Let [Def (LIdent "hello") (Var (LIdent "nooo"))] (Var (LIdent "hello"))
    let2   = Apply (Lambda [LIdent "hello"] (Var (LIdent "hello"))) [Var (LIdent "nooo")]
    zero   = Apply (Const (UIdent "Z")) []
    one    = Apply (Const (UIdent "S")) [Apply (Const (UIdent "Z")) []]

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
  | otherwise        = lookupBr ident2 branches

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

casenat :: Exp -> a -> (Exp -> a) -> a
casenat (Apply (Const (UIdent "Z")) []) b c     = b
casenat (Apply (Const (UIdent "S")) (n:[])) b c = c n
cosenat _ _ _                                   = error "casenat is only defined for N"

codenat :: Int -> Exp
codenat 0     = Apply (Const (UIdent "Z")) []
codenat n
  | n < 0     = error "not defined for negative values"
  | otherwise = (Apply (Const (UIdent "S")) [(codenat (n - 1))])

decodenat :: Exp -> Int
decodenat (Apply (Const (UIdent "Z")) [])     = 0
decodenat (Apply (Const (UIdent "S")) (x:[])) = 1 + (decodenat x)
decodenat _                                   = error "undefined for given argumnets"

add = parse "rec add = \\x y. case x of { Z -> y; S -> \\n. (S (add n y))}"

-------------------------------------
printExp e = putStr ((printTree e)++"\n")
parse :: String -> Exp
parse s = case (pExp (myLexer s)) of
           Bad s2   -> error ("\nParse Failed...\n   " ++ s2)
           Ok  tree -> tree 
parsef file = do
        pString <- (readFile file) 
        printExp (parse pString)