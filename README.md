# Functional-Program-Design-in-Scala-Notes

## Week 1: For Expressions and Monads

### Recap: Functions and Pattern Matching

Case Classes: Case Classes in Scala are a way to define complex data.
example: Representing JSON
   
    {
        "firstName": "aniket",
    
        "lastName": "joshi" 
    
        "address": {
            "streetAddress": "21 2nd Street",
            "state": "OR",
            "postalCode": 90210
        },
    
        "phoneNumbers": [
            { "type": "home", "number": "100 200-400" },
            {"type": "fax", "number": "123 456-9900 "}
        ]
    }
    
### Representation of JSON in Scala
    
    abstract class JSON
    
    case class JSeq(elems: List[JSON])  extends JSON
    case class JObj(elems: List[JSON])  extends JSON
    case class JNull extends JSON

    
### Function are Objects

In Scala, every concrete type is the type of some class or trait, and so is the function type.
For example, a type like JBinding => String is a shorthand for `scala.Function1[JBinding, String]` where `scala.Function1` is a trait 
and `JBinding` and `String` are its types

### Subclassing Functions

One nice aspect of functions being traits is that we can subclass the function type, for example maps are functions from keys to values
`trait Map[Key, Value] extends (Key => Value)`

Sequences are functions from indices of type Int to values
`trait Seq[Elem] extends (Int => Elem)` that is why we can write `elem(i)`

### Partial Matches

We have seen that a pattern matching block looks like

`{ case "ping" => "pong" }` can be given a type `String => String`
`val f: String => String = { case "ping" => "pong" }`

But the function is not defined on all in its domain!
`f("pong") //gives Match Error`

Is there a way to find out whether the function can be applied to a given argument before running it?
yes, with the help of partialFunctions

`val f: PartialFunction[String, String] = { case "ping" => "pong" }`

f.isDefinedAt("ping") //true
f.isDefinedAt("pong") //false
 
trait for partialFunction

```
trait PartialFunction[-A, +R] extends Function1[-A, +R] {
 def apply(x: A): R
 def isDefinedAt(x: A): Boolean
}
```
  
## Recap on Scala Collections
Scala has a rich hierarchy of collection classes

core methods:
map, flatMap, filter are defined for arbitrary collections, not just lists
and also foldLeft, foldRight

### For-Expressions
Simplify combinations of core methods map, flatMap, filter

Eg:

```
(1 until n) flatMap (i =>
 (1 until i) filter(j => isPrime(i + j)) map
 (j => (i, j))
```
 one can write 
 
 ```
     for {
        i <- 1 until n
        j <- 1 until i
        if isPrime(i + j)
     } yield(i, j)
 ```
 
 1. A simple for-expression
 
    `for (x <- e1) yield e2` is translated to `e1.map(x => e2)`
 
 2. A for-expression
 
     `for (x <- e1; if f; s) yield e2` where `f` is a filter and `s` is a (potentially empty) sequence of generators and filters, is translated to
     `for (x <- e1.withFilter(x => f); s) yield e2`
 
 3. A for-expression
 
     `for (x <- e1; y <- e2; s) yield e3` where `s` is a (potentially empty) sequence of generators and filters, is translated to `e1.flatMap(x => for(y <- e2); s) yield e3`
     
 
## For-expressions and Pattern matching:
 The left-hand side of a generator may also be a pattern
 
 eg:
 
```
  val data: List[JSON] = ... 
  
 for {
    JObj(bindings) <- data
    JSeq(phones) = bindings("phoneNumbers")
    JObj(phone) <- phones
    JStr(digits) = phone("number")
    if digits startsWith "212" 
 } yield (bindings("firstName"), bindings("lastName"))
 
```
 
### Translation of Pattern Matching in For:
 
 If `pat` is a pattern with a single variable `x`, we translate
  `pat <- expr`
 
```
     x <- expr withFilter {
        case pat => true
        case _ => false 
     } map {
        case pat => x
     }
```
 Example:
```
     for {
        x <- 2 to N
        y <- 2 to x
        if(x % y) == 0
     } yield (x, y)
```
 will be mapped to:
 
``` 
     (2 to N) flatMap ( x =>
        (2 to x) withFilter( y =>
            x % y == 0) map (
            y => (x, y))
```
 
## Queries with For:
 The for notation is essentially equivalent to the common operations of query languages for databases
  
 Eg: we have a database of books, represented as List of books
 `case class Book(title: String, authors: List[String])`
 
```
    val books: List[Book] = List (
        Book (title  = "Structure and Interpretation of Computer Programs",
             authors = List("Abelson, Herald", "Sussman, Gerald J.")),
        Book(title   = "Introduction to Functional Programming",
             authors = List("Bird, Richard", "Wadler, Phil")))
```
 
## Some queries:
 
 1. find titles of books whose author's name is "Bird":
 
    `for (b <- books; a <- b.authors if a startsWith "Bird,") yield b.title`
 
 2. find all the book which have the word "Program" in the title:
 
    `for (b <- books if b.title indexOf "Program" >= 0) yield b.title`
    
 3. find the names of all authors ho have written atleast 2 books in the present
    
```
    for {
     b1 <- books
     b2 <- books
     if b1 != b2
     a1 <- b1.authors
     a2 <- b2.authors
     if a1 == a2
    } yield a1
```
    
 multiple ways to get rid of duplicate authors showing up in the results:
 either filter on b1.title < b2.title, but if the author has 3 books, we would see a List containing the same author's name three times.
 or
 we can wrap the entire query and pull out `distinct` records.
 or
 we can make use of a `Set` instead of `List` to store the collection of all the `Book` case class
 
 
### Translation of For
 
 In reality scala compiler expresses for-expressions in terms of `map`, `flatMap` and a lazy variant of `filter`
 
 1. `for (x <- e1) yield e2` is translated to `e1.map(x => e2)`
 
 2. `for (x <- e1; if f; s) yield e2` is translated to `for (x <- e1.withFilter(x => f); s) yield e2`. `withFilter` is a variant of `filter` that does not
 produce and intermediate list, but instead filters the following `map` or `flatMap` function application
 
 3. `for (x <- e1; y <- e2; s) yield e3` is translated to `e1.flatMap(x => for (y <- e2; s) yield e3)`
 eg:
 translate `for (b <- books; a <- b.authors if a startsWith "Bird") yield b.title` into a higher-order function

 `books.flatMap(books => for (author <- books.authors; if author startsWith "Bird") yield books.title)`
 
 or
 
 `books.flatMap(books => for (author <- books.authors withFilter(author => author startsWith "Bird")) yield books.title)`
  now, since we have 1 generator we can use a `map`
  

### Functional random Generators:
  
We can use for expressions not only on collections but also on random number generations
We can generate a integer random number as:
```

    import java.util.Random
    val rand = new Random
    rand.nextInt()

```
What is a systematic way to get random values for other domains such as booleans, strings, tuples, lists, set, trees ?

### Generators
we define a trait Generator[T] that generates random value of type T:
```
    trait Generator[+T] {
     def generate: T
    }
```

Some instances:
```

    val integers = Generator[Int] {
        val rand = new java.util.Random
        def generate = rand.nextInt()
   }

    val booleans = Generator[Boolean] {
        def generate = integers.generate > 0 //note how we used random integers from above
    }
    
    val pairs = Generator[(Int, Int)] {
        def generate = (integers.generate, integers.generate)
    }
    
```
### How do we avoid new Generator boilerplate?
the compiler converts the above lines into map, and flatMap(in case of pairs) on the Generator class.

### Generator with map and flatMap
```

    trait Generator[+T] {
    self =>
    
    def generate: T
    def map[S](f: T => S): Generator[S] = new Generator[S] {
        def generate = f(self.generate)
    }
    
    def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
            def generate = f(self.generate).generate
        }
        
     //more examples
    
    def single[T](x: T): Generator[T] = new Generator[T] {
        def generate = x
      }
     
     def choose(lo: Int, hi: Int): Generator[Int]= for(x <- integers) yield lo + x % (hi - lo)
     def oneOf[T](xs: T*): Generator[T] = for(idx <- choose(0, xs.length)) yield xs(idx) 
    }
```

### A List Generator
```

    def lists: Generator[List[Int]] = for {
        isEmpty <- booleans
        list <- if(isEmpty) emptyLists else nonEmptyLists
    } yield list

    def emptyLists = single(Nil)
    
    def nonEmptyLists = for {
     head <- integers
     //recursively call lists
     tail <- lists
    } yield head :: tail
```

### Tree generator
Tree is either a leaf or an inner node
```

    trait Tree
    case class Inner(left: Tree, right: Tree) extends Tree
    case class Leaf(x: Int) extends Tree
    
    def leafs: Generator[Leaf] = for {
     x <- integers
    } yield Leaf(x)
    
    def inners: Generator[Inner] = for {
     left <- trees
     right <- trees
    } yield Inner(left, right)
    
    def trees: Generator[Tree] = for {
     isLeaf <- booleans
     tree <- if(isLeaf) leafs else inners
    } yield tree
```

# Monads

### What is a Monad?

A Monad M is a parametric type M[T] with 2 operations, `flatMap` and `unit`, that have to satisfy some laws.

```
trait M[T] {
 def flatMap[U](f: T => M[U]): M[U]
}
def unit[T](x: T): M[T]
```

`map` can be defined for every monad as a combination of `flatMap` and `unit`:

```
m map f == m flatMap (x => unit(f(x)))
        == m flatMap (f andThen unit)
```

### Monad Laws:

1. Associativity
`(m flatMap f) flatMap g == m flatMap (x => f(x) flatMap g)`

2. Left Unit
`unit(x) flatMap f == f(x)`

3. Right Unit
`m flatMap unit == m`

Eg: Monad laws for Option

```
abstract class Option[+T] {
 def flatMap[U](f: T => Option[U]): Option[U] = this match {
   case Some(x) => f(x)
   case None => None
  }
 }
```

### checking Left Unit Law:

`Some(x) flatMap f == f(x)`

```
we expand what flatMap means
== Some(x) match {
    case Some(x) => f(x)
    case None => None
}
== f(x)
```

### checking the Right Unit Law:
we need to show
`opt flatMap Some == opt`
`opt flatMap Some`

//i.e. some optional value flatMap with Some(which is the unit constructor) gives the same thing as that optional value

```
== opt match {
    case Some(x) => Some(x)
    case None => None
   }
== opt   
```

### Checking the Associative Law:
we need to show: 

`opt flatMap f flatMap g == opt flatMap (x => f(x) flatMap g)`
consider the left hand side first:

```
opt flatMap f flatMap g

== opt match { case Some(x) => f(x) case None => None }
       match { case Some(y) => g(y) case None => None }

we will mix in the second match into the first as follows:

== opt match {
  case Some(x) => f(x) match { case Some(y) => g(y) case None => None }
  case None => None match { case Some(y) => g(y) case None => None }
 }
 
 if we look at None case, it will match the case None inside, simplifying the above expression to

== opt match {
   case Some(x) => f(x) match { case Some(y) => g(y) case None => None }
   case None => None
  }
 
 if we consider the first case of Some(x), the f(x) matches nothing but flatMap g, which can be rewritten as:

== opt match {
   case Some(x) => f(x) flatMap g //where flatMap g == case Some(y) => g(y) case None => None 
   case None => None
  }
  
== opt flatMap (x => f(x) flatMap g) == R.H.S  
```

### Significance of the Laws for For-Expressions
- Associativity says that one can "inline" a nested for-expression

eg: 
`for (y <- for (x <- m; y <- f(x)) yield y z <- g(y)) yield z`

can be rewritten as:

```
for (x <- m;
     y <- f(x)
     z <- g(y)) yield z
```
- Right unit says
`for (x <-m) yield x` == `m`

- Left unit does not have an analogue for for-expressions

### Another type: Try

Try resembles an Option but instead of Some/None there is a `Success` case that returns with a value and a `Failure` case
 that contains an exception:
 
```
abstract class Try[+T]
case class Success[T](x: T)        extends Try[T]
case class Failure(ex: Exception)  extends Try[Nothing]
```

 Try is used to pass results of computations that can fail with an exception between threads or computers
 Try-valued computations can be composed in for expressions
 
```
    for {
        x <- computeX
        y <- computeY
    } yield f(x, y)
```
what it means is if computeX and computeY both succeed with results `Success(x)` and `Success(y)`, this will return
`Success(f(x, y))`.
If either computation fails with an exception `ex`, this will return `Failure(ex)`.

### is Try a monad?
 Try is not a monad since, the left unit law fails
 
 `Try(exp) flatMap f != f(expr)`
 the L.H.S will never raise a non-fatal exception whereas the R.H.S will raise any exception thrown by `expr` of `f`
 
 Hence, Try trades one monad law which is more useful in this context
 
 "An expression composed from `Try`, `map` or `flatMap` will never throw a non-fatal exception"
 
### Conclusion
- for-expressions are useful not only for collections many other types also define `map`, `flatMap`, and `withFilter`
 operations and with them for-expressions, example: `Generator`, `Try`, `Option`
-  many of the types defining `flatMap` are `monads`, if they also define `withFilter` they are also called "monads with zero"

- the three monad laws give useful guidance in the design of library of APIs
 