# Week 1

## 1

Let f be a function whose domain is a set A. The function f is injective if and only if for all a and b in A, if f(a) = f(b), then a = b; that is, f(a) = f(b) implies a = b.  Equivalently, if a ≠ b, then f(a) ≠ f(b).
Or in predicate logic  (forall a,b:A) f(a) = f(b) => a = b, which is logically equivalent to the contrapositive  (forall a,b:A) a ≠ b => f(a) ≠ f(b).

A partial function f : A ↛ B is a function f : A' → B, for some subset A' of A. It generalizes the concept of a function f : A → B by not forcing f to map every element of A to an element of B (only some subset A' of A).

If A' = A, then f is called a total function and is equivalent to a function.

## 2

We can use the diagonalization argument to show that all rational numbers are enumerable using
the table T below.

```
f_1 | 1/1 | 1/2 | 1/3 | 1/4 | ...
f_2 | 2/1 | 2/2 | 2/3 | 2/4 | ...
f_3 | 3/1 | 3/2 | 3/3 | 3/4 | ...
f_3 | 4/1 | 4/2 | 4/3 | 4/4 | ...
...
```

Increment dia(i) by one; this will get us a set of the value 2. Which is already in the table on row 2 column 1. We can also see that it doesn't matter if we add any natrual number > 0, as this value will allways be avalible at the same line as the quanity we added + 1. So if we add 4, row 5 column 1 will contain the value. To make n applicable for any rational number we can think about it like this.

```
a = numerator of (n + 1)
b = denominator of (n + 1)
dia(i) + n == T[a][b]
                   ^ column
                ^ row
```

This shows that all values are already in our table no matter what rational number we assign n thus proving that all rational numbers are enumerable.

## 3

While having “a subset of an enumerable set is enumerable” in mind the following should be true:
Programs are stored in the computer as a text string, each character having its ascii code. We can look at this string as a binary number. So each program can be seen as a number, hence the set of all programs can be seen as a subset of all natural numbers. Hence it must be enumerable.

The set N -> N is not countable. If it were, we could find an enumeration f1, f2, …, of all its elements. But then the function defined by diag(i) = fi(i) + 1 cannot be in the set, since if it were, we must have that diag = fj, for some number j. But this is impossible, since diag(j) != fj(j). This diagonalization argument shows that there are more functions in N -> N than programs taking a natural number to a natural number.
