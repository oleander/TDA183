# Week 3

## 1

We'll first show that the given expression can be reduced to fst n (0 1), where n is a Church
numeral.

```
-- fst is defined to return the first argument of a pair
-- snd is defined to return the last argument of a pair
-- succ is defined to return the successor of its argument

zz == pair 0 0
ss == λn. pair (snd p) (succ (snd p))
prd == λn. fst (n ss zz)

-- We pass zz as an argument to ss which now can be expressed as
ss zz == pair (snd pair (0 0) succ (snd (pair 0 0))) =>
ss zz == pair (0 (succ 0)) =>
ss zz == pair (0 1) =>

-- prd can now be expressed as
prd == λn. fst (n pair 0 1)
```

Next step is to find pattern. What happens when you apply `pair 0 1` to n?

```
-- Lets test with an arbitrary value for n, say 1
prd 1 == fst (1 pair 0 1)

-- One and zero can be expressed as Church`s numbers
0 == λp.λa.a
1 == λr.λp.r p

-- Removing "fst" for now and substituting 0 and 1 with Church´s numbers we now
have
(λr.λp.r p) (λf.f (λp.λa.a) (λr.λp.r p))

-- This can be reduced to
λp.(λf.f (λu.λa.a) (λr.λu.r u)) p =>
λf.f (λp.λa.a) (λr.λp.r p) =>
λf.f (λp.λa.a) (λr.r)

-- We now see that applying "fst" to the above statement will yield "0", which is
the predecessor of "1". We can see that by applying "(0 1)" to any Church number
"n", the expression can be reduced to (n - 0 - 1, n - 1 - 1). Taking the first
argument of this pair yields the the correct value. Applying `5 3` on `n` would for
example yield `(n - 5 - 1, n - 3 - 1)`.
fst λf.f (λp.λa.a) (λr.r) =>
(λp.λa.a) -- Equals zero, which is the predecessor of 1
```

## 2

We saw in the previous answer that by applying a Church number a to another number b we get a
- b - 1. Which is almost what we want. Instead we want to find an expression that cancels the -1
expression so that we end up with a - b. This can be done by first applying prd to b before
applying it to a.

```
prd b == b - 1
a - b =>
a - (b - 1) - 1 =>
a - (prd b) - 1
```

We can now express sub a b like this.

```
sub == λa.(λb a prd b)
```

Note that if `a` is less then `b`, zero will be returned. As sub is only defined for `N -> N -> N`.

## 3

```
-- Church booleans
true == λx.λy.x
false == λx.λy.y

iszero == (λn.n)(λn.false) true
equal a b == λa.(λb iszero sub a b)
```

The idea is that if two numbers are equal, the differential should be zero. iszero takes the
differential and returns true if the value is zero otherwise false. This is done by first passing
λn.false as an argument to the ingoing value. If it's zero, true is passed up the chain and returned. If instead the argument is a non zero value the argument λx.false will be passed to iszero´s argument, which is then applied to Church zero which is finally thrown away. Leaving on false to be returned. The problem now is that sub isn't defined for Z, meaning that if a is less then b it will return zero and equal will return true. We can solve this by checking that both equal a b and equal b a is true.

```
and == λp.λq.p q p
equalA a b == λa.(λb iszero sub a b)
equal a b == an
```

Now both `a - b` and `b - a` has to be zero for equal to return true. We can see that and works by
applying each boolean value to another (accordning to the definition of and above) like this; true
true true, false true false, true false true and false false false.

```
true == true true true
false == false true false
false == true false true
false == false false false
```

## 4

We can define the set of free variables like this.

- FV(x) = {x}, where x is a variable
- FV(λx.M) = FV(M) \ {x}
- FV(M N) = FV(M) U FV(N)
- FV(c) = {}, where c is a constant

The question states that no free variables occurs in the expression d. The above definition states that a variable x is always free, meaning that a variable x on its own can't exist in d.

- (λx.s)[t/x] = (λx.s), as FV(s) = {}
- (λy.s)[t/x] = (λy.s), as FV(s) = {}
- (a b)[t/x] = (a b), as FV(a) U FV(b) = {}
- c[t/x] = c, as FV(c) = {}

No other substitutions can occur as we're not allowed to substitute non free variables. No other
lambda expression can exist either as all variables in (λy.s) has to be bound. So four the above
rules above are enough.