## Week 2

## Introduction

The submission consists of one file, excuding this report, written in Haskell Main.hs.
The code can be compile and executed using ghc -o app Main.hs && ./app.

## 1

See Main.hs for the implementation of N.

## 2

See Main.hs the implementation of add. Example: main = print (add (Succ Zero) (Succ Zero))
Take a look at the implementation in Main.hs for more details.

## 3

See Main.hs for the implementation of Z.
The integers -1, 0, 1 and 3 can be represented using the Succ, Zero, Neg och Pos constructors. The
idea is to recursively decrement (or increment depending on the value) until we end up in Zero.

```
-1 : Neg (Succ Zero)
0 : Pos Zero
1 : Pos (Succ Zero)
3 : Pos (Succ (Succ (Succ Zero)))
```

We can build 3 like this.

```
3 =>
Pos 3 =>
Pos (Succ 2) =>
Pos (Succ (Succ 1)) =>
Pos (Succ (Succ (Succ 0))) =>
Pos (Succ (Succ (Succ Zero)))
```

We can build 1 like this.

```
1 =>
Pos 1 =>
Pos (Succ 0) =>Pos (Succ Zero)
We can build 0 like this.
0 =>
Pos 0 =>
Pos Zero
```

and finally -1 like this.

```
-1 =>
Neg 1 =>
Neg (Succ 0) =>
Neg (Succ Zero)
```

We can easily extend N and Z with addition, subtraction and multiplication using the instance
declaration in Haskell.

```
instance Num N where
 a * b = undefined -- add def of (*) here
 a + b = undefined -- add def of (+) here
 a - b = undefined -- add def of (-) here
```

Note also that this definition of Z is normalized, meaning that an element in Z can only be created in one way.

## 4

See Main.hs for the implementation of zrec.

## 5

See Main.hs for the implementation of ntoz. ntoz is a surjective function and is thus enumerable as all elements in N are mapped to all values in Z. It's defined as this.

```
ntoz =
 x / 2 if x is even
 (1 - x) / 2 if x is odd
```

All even numbers in N will map the set Z+, `[0, 1, 2, 3, 4, ...]`. All odd numbers in N will map the set Z-, `[0, -1, -2, -3, -4, ...]`. Together they map the whole range of Z.