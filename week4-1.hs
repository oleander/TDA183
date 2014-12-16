type PRFs = [Int] -> Int

-- Should work
main = print "Uncomment any of the below examples"
--main = print (show (mul [5, 5])) -- 25
--main = print (show (fact [3])) -- 6
--main = print (show (add [0, 9])) -- 9
--main = print (show (pre [6])) -- 5
--main = print (show (suc [4])) -- 5

--  Should fail
--main = print (show (mul [10, -1]))
--main = print (show (mul [-1, 5]))
--main = print (show (mul [1, -1]))
--main = print (show (mul [1]))
--main = print (show (add [1]))
--main = print (show (fact []))
--main = print (show (fact [-1]))

-- Start: Zero
zero :: PRFs
zero = compose (proj 0) [retZero]

retZero :: [Int] -> Int
retZero [] = 0
retZero _ = error "to many arguments to zero"
-- End: Zero

-- Start: Suc
suc :: PRFs
suc = compose (proj 0) [plusOne]

plusOne :: [Int] -> Int
plusOne [] = error "too few argument to suc"
plusOne (n:[]) 
  | n < 0 = error "suc isn't defined for negative values"
  | otherwise = n + 1
plusOne _ = error "too many arguments to suc"
-- End: Suc

-- Start: proj
proj :: Int -> PRFs
proj n
  | n < 0 = error "negative index to proj isn't allowed"
  | otherwise = (\xs -> fetchElemFromList xs n)

-- Raises an error if n > xs.length
fetchElemFromList :: [Int] -> Int -> Int
fetchElemFromList [] _ = error "invalid argument to proj"
fetchElemFromList (x:xs) 0 = x
fetchElemFromList (x:xs) n = fetchElemFromList xs (n - 1)
-- End: proj

-- Start: compose
-- Example: (compose a [b, c]) [1, 2] => a([b([1,2]), c[1,2]])
compose  :: PRFs -> [PRFs] -> PRFs
compose  g hs = (\xs -> g([(h xs) | h <- hs]))
-- End: compose

-- Start: rec
rec      :: PRFs -> PRFs -> PRFs
rec g h = (\xs -> recHelper g h xs)

recHelper :: PRFs -> PRFs -> [Int] -> Int
recHelper g h [] = error "rec can't handle an empty list"
recHelper g h (0:xs) = g (checkIfValid xs [])
recHelper g h (x:xs)
  | x < 0     = error "first arg to rec can't be negative"
  | otherwise = h ((x - 1):(recHelper (g) (h) ((x - 1):xs)):xs)

-- Checkes to see if all values in list are >= 0
checkIfValid :: [Int] -> [Int] -> [Int]
checkIfValid [] ys = ys
checkIfValid (x:xs) ys
  | x < 0     = error "list can't contain negative values"
  | otherwise = checkIfValid xs (ys ++ [x])
-- End: rec

add = rec (proj 0)(compose suc [proj 1])
pre = rec (zero) (proj 0)
mul = rec (compose zero []) (compose add [(proj 1), (proj 2)])
fact = rec (compose suc [zero])
         (compose mul [compose suc [proj 0],
                           proj 1])