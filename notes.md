Chapter 1
===

Functional programming == programming with pure functions

Pure functions
---

Functions with no side effects. 

Formal definition: a function with no side effects f(x) is referentially transparent for all
referential transparent x.

A expression is referential transparent in a program if it can be replaced by the result
of the evaluation of the expression without modifying the final result of the program.

Example of pure function:

```
scala> val s = "Hello"
s: String = Hello

scala> val e1 = s.concat(" World").concat("!")
e1: String = Hello World!

scala> val e2 = "Hello Wolrd".concat("!")
e2: String = Hello Wolrd!

scala> val e3 = "Hello World".concat("!")
e3: String = Hello World!
```

Since the expressions result on the same output, the calls the expressions are referential transparent and the concat
is probably a pure functions (it cannot be proved so simple like that)


Example of impure function:

```
scala> val sb = new StringBuilder("Hello")
sb: StringBuilder = Hello

scala> val e1 = sb.append(" World!")
e1: StringBuilder = Hello World!

scala> val e2 = sb.append(" World!")
e2: StringBuilder = Hello World! World!

scala> val e3 = new StringBuilder("Hello").append(" World!")
e3: StringBuilder = Hello World!
```

Since e2 != e3 and e1 != e2, the expressions cannot be referential transparent, so append is not a pure function.


Usually, pure functions are easier to reason about because we don't need to mentally store the value of variables
or care about any external state.


Chapter 2
===

**Polymorphic functions**: Functions with type parameters, example
```scala
def isSorted[T](items: Array[T], greaterThan: (T,T)=>Boolean): Boolean = {...}
```