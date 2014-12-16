-- NOTE: I went with creating a report.pdf instead of 
-- writing my comments/answers here as the code below 
-- quickly became smuddered with comments. I hope that's okay.

data N = Zero | Succ N deriving(Eq, Show, Ord)

instance Num N where
  fromInteger a = convertToInteger a

data Z = Pos N | Neg N deriving(Eq, Show, Ord)
instance Num Z where
  fromInteger a = initZ a

-- Helper function for instance Num Z
convertToInteger :: Integer -> N
convertToInteger 0 = Zero
convertToInteger n = Succ (convertToInteger (n - 1))

initZ :: Integer -> Z
initZ a
  | a > 0 = Pos (convertToInteger a)
  | a < 0 = Neg (convertToInteger (abs a))

--main = print (zrec (Pos (Succ Zero)) (Pos Zero) ee)
--main = print (add (Succ Zero) (Succ Zero))
main = print (add (Succ Zero) (Zero))
--main = print (ntoz (Succ Zero))

-- Helper function when using zrec in main
-- Ads one to second argument
ee :: Z -> Z -> Z
ee a (Neg (Succ b)) = Neg b
ee a (Pos b) = Pos (Succ b)

ntoz :: N -> Z
ntoz n 
  | isOdd(n) = normalize (divNegByTwo (plusOne (negateN n)))
  | otherwise = normalize (Pos (divByTwo n))

-- Z should not return both + and - zero
normalize :: Z -> Z
normalize (Pos Zero) = Pos Zero
normalize (Neg Zero) = Pos Zero
normalize a = a

-- Start: Implements add for N
add :: N -> N -> N
add a b = rec a b e

e :: N -> N -> N
e a b = Succ b

rec :: N -> c -> (N -> c -> c) -> c
rec Zero d e = d
rec (Succ x) d e = e x (rec x d e)
-- End: Implements add for N

-- Start : Check if N is odd or not
isOdd :: N -> Bool
isOdd n = isOddHelper n 0

isOddHelper :: N -> Integer -> Bool
isOddHelper Zero n = (mod n 2) == 1 -- Is odd
isOddHelper (Succ a) n = isOddHelper a (n + 1)
-- End: Check if N is odd or not

-- Start: Divide a negative number by two
divNegByTwo :: Z -> Z
divNegByTwo (Neg n) = Neg (divByTwo n)
divNegByTwo (Pos n) = error "not defined for pos values"
-- End: Divide a negative number by two

-- Start : Divide N by 2
divByTwo :: N -> N
divByTwo n 
  | isOdd(n) = error "value can't be odd"
  | otherwise = divByTwoHelper n 0

divByTwoHelper :: N -> Integer -> N
divByTwoHelper Zero n = buildHelper (div n 2)
divByTwoHelper (Succ a) n = divByTwoHelper a (n + 1)

buildHelper :: Integer -> N
buildHelper 0 = Zero
buildHelper n = Succ (buildHelper (n - 1))
-- End : Divide N by 2

-- Start : Divide Z by 2
divByTwoZ :: Z -> Z
divByTwoZ (Neg n) = Neg (divByTwo n)
divByTwoZ (Pos n) = Pos (divByTwo n)
-- End : Divide Z by 2

-- Start: Negates N
negateN :: N -> Z
negateN n = Neg n
-- End: Negates N

-- Start: Adds (1) to a negative number
plusOne :: Z -> Z
plusOne (Neg (Succ n)) = Neg n
plusOne otherwise = error "not defined for > 0"
-- End: Adds (1) to a negative number

zrec :: Z -> c -> (Z -> c -> c) -> c
zrec (Pos Zero) d e = d
zrec (Neg Zero) d e = d
zrec (Neg (Succ x)) d e = zrec (Neg x) d e
zrec (Pos (Succ x)) d e = zrec (Pos x) d e
Vimium has been updated to 1.48.×