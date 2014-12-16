# Week 1

## 1

f : A -› B to be injective if (forall y:B)(forall x:A) f(x) = f(y) ==> x = y
f : A -› B is a total function if (forall x:A) (exists y:B) f(x) = y
f : A -› B is a partial function if (exists x:A) (exists y:B) f(x) = y

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

## 3.A

A C program can be represented as a list of binary values, which in it self is a list of numbers. Which means that've a set of all functions from C programs -> N. By definition any set that injects into N is countable, thus showing that all C programs are enumerable.


## 3.B

We need to show that not all functions can be expressed as programs by disproving N -> programs.
Again, all programs is a subset of N, and N -> N is not enumerable, which means that N ->
programs isn't eather. We know this as there will be more missing values in our table when using
the diagonalization argument on N -> programs as all programs are a subset of all numbers.
Proving that N -> N is not enumerable can be done using the diagonalization argument. dia(i) +
1 will not be part of the set f_i(n) for all n in N and for all i in N.