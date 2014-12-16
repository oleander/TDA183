type PRFs = [Int] -> Int
data PRF = Zero | Succ | Proj Int | Rec PRF PRF | Compose PRF [PRF] 
  deriving Show
eval :: PRF -> PRFs

-- Should work
main = print "Uncomment any of the below examples"
--main = print (show (eval mul [5, 5])) -- 25
--main = print (show (eval add [0, 9])) -- 9
--main = print (show (eval pre [6])) -- 5
--main = print (show (eval Succ [4])) -- 5
--main = print (show (eval fact [3])) -- 6

--  Should fail
--main = print (show (eval mul [10, -1]))
--main = print (show (eval mul [-1, 5]))
--main = print (show (eval mul [1, -1]))
--main = print (show (eval mul [1]))
--main = print (show (eval add [1]))
--main = print (show (eval fact []))
--main = print (show (eval fact [-5]))

-- Start: zero
eval Zero [] = 0
eval Zero xs = error "to many args to zero"
-- End: zero

-- Start: succ
eval Succ [x] 
  | x < 0 = error "argument to succ can't be negative"
  | otherwise = x + 1
eval Succ _ = error "to many args to succ"
-- End: succ

-- Start: proj
eval (Proj n) xs 
  | n < 0 = error "negative index to proj isn't allowed"
  | otherwise = fetchElemFromList xs n
-- End: proj

-- Start: compose
eval (Compose g hs) xs = eval g (map (\gg -> eval gg xs) hs)
-- End: compose

-- Start: rec
eval (Rec g h) (0:xs) = eval g (checkIfValid xs [])
eval (Rec g h) [] = error "rec can't handle an empty list" 
eval (Rec g h) (x:xs) 
  | x < 0     = error "args to rec can't be negative"
  | otherwise = eval h ((x - 1):eval (Rec g h) ((x - 1):xs):xs)

checkIfValid :: [Int] -> [Int] -> [Int]
checkIfValid [] ys = ys
checkIfValid (x:xs) ys
  | x < 0     = error "list can't contain negative values"
  | otherwise = checkIfValid xs (ys ++ [x])
-- End: rec

-- Raises an error if n > xs.length
fetchElemFromList :: [Int] -> Int -> Int
fetchElemFromList [] _ = error "invalid argument to proj"
fetchElemFromList (x:xs) 0 = x
fetchElemFromList (x:xs) n = fetchElemFromList xs (n - 1)

add = Rec (Proj 0) (Compose Succ [Proj 1])
pre = Rec (Zero) (Proj 0)
mul = Rec (Compose Zero []) (Compose add [Proj 1, Proj 2])
fact = Rec (Compose Succ [Zero])
         (Compose mul [Compose Succ [Proj 0],
                           Proj 1])