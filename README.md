A compiler/interpreter of a basic lazy functional language, written in Scala.

Based on the classic book [Implementing functional languages: a tutorial](https://www.microsoft.com/en-us/research/publication/implementing-functional-languages-a-tutorial/).

---

Some example programs that this interpreter can evaluate are given below.

The first 10 Fibonacci numbers obtained by zipping the infinite list of all Fibonacci numbers with itself:

```
fib = Cons 0 (Cons 1 (zipWith (λa b.a+b) fib (tail fib))) ;
main = printList (take 10 (drop 1 fib))
```

The first 10 prime numbers found with the sieve of Eratosthenes algorithm:

```
from n = Cons n (from (n+1)) ;

nonMultiple p n = ((n/p)*p) != n ;

sieve xs = case xs of
       <1> -> nil ;
       <2> p ps -> Cons p (sieve (filter (nonMultiple p) ps)) ;

main = printList (take 10 (sieve (from 2)))
```