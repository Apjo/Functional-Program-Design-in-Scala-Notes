# Functional-Program-Design-in-Scala-Notes
These are the notes I prepared while doing the online course Functional-Program-Design-in-Scala- from [coursera](https://www.coursera.org/learn/progfun2/). Hope this helps!
# Week 1: For Expressions and Monads

## Recap: Functions and Pattern Matching

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

    
## Function are Objects

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
 
# Week 2

## Structural Induction on Trees
Structural induction is not limited to lists; it applies to any tree structure
The general induction principle is as follows:
To prove a property `P(t)` for all trees `t` of a certain type

- show that `P(l)` holds for all leaves `l` of a tree
- for each type of internal node `t` with subtrees `s1,...,sn` show that
  `P(s1) ^ ... ^ P(sn) implies P(t)`

### Laws of IntSet
proving correctness of an implementation in this case for IntSet,
by proving the laws that it respects
definition of IntSet

```
abstract class IntSet {
 def incl(x: Int): IntSet
 def contains(x: Int): Boolean
}

object Empty extends IntSet {
 def contains(x: Int): Boolean = false
 def incl(x: Int): IntSet = NonEmpty(x, Empty, Empty)
}
```
- Empty contains x      = false
- (s incl x) contains x = true
- (s incl x) contains y = s contains y if x != y

### Proving of the Laws of IntSet
1. Empty contains x = false
as per our definition of `contains` for the object `Empty` it returns false, hence proved.

2. (s incl x) contains x = true
Proof by structural induction on s
Base case: Empty
(Empty incl x) contains x

by definition of Empty.incl
**NonEmpty(x, Empty, Empty) contains x**
== true //by definition of NonEmpty.contains

**induction step: NonEmpty(x, l ,r)**
case: root element x is the same as the one we included x

(NonEmpty(x, l, r) incl x) contains x
= NonEmpty(x, l, r) contains x //definition of NonEmpty.incl
= true //by definition of NonEmpty.contains

**Induction step: NonEmpty(y, l, r) where y < x**
(NonEmpty(y, l, r) incl x) contains x
//x > y so we have a recursive include at the right element of y
= NonEmpty(y, l, r, incl x) contains x //definition of NonEmpty.incl
= (r incl x) contains x //definition of NonEmpty.contains
= true //induction hypothesis

**Induction step: NonEmpty(y, l, r) where y > x** analogous to previous one

3. if x != y then
(xs incl y) contains x = xs contains x
Assume y < x, y > x will be analogous

**Base case: Empty**
(Empty incl y) contains x //to show Empty contains x
= NonEmpty(y, Empty, Empty) contains x // as per definition of Empty.incl
= Empty contains x //as per definition of NonEmpty.contains, we go to the right subtree since x > y, hence the Empty

For the inductive step, we need to consider a tree NonEmpty(z, l ,r), we have 5 cases:

1. z = x
2. z = y
3. z < y < x
4. y < z < x
5. y < x < z

Consider  case: **z = x and z = y**
**Induction step: NonEmpty(x, l ,r)**
(NonEmpty(x, l, r) incl y) contains x // to show = NonEmpty(x, l, r) contains x
= NonEmpty(x, l incl y, r) contains x // by definition of NonEmpty.incl
= true //by definition of NonEmpty.contains
= NonEmpty(x, l, r) contains x

**Induction step: NonEmpty(y, l ,r)**
(NonEmpty(y, l, r) incl y) contains x // to show = NonEmpty(y, l, r) contains x
 = NonEmpty(y, l, r) contains x
 
Now, consider case: **z < y**
**Induction step: NonEmpty(z, l ,r) where z < y < x**
(NonEmpty(z, l ,r) incl y )contains x // to show = NonEmpty(z, l ,r) contains x
= NonEmpty(z, l, r incl y) contains x // by definition of NonEmpty.incl
= (r incl y) contains x // by definition of NonEmpty.contains
= r contains x // by induction hypothesis
= NonEmpty(z, l ,r) contains x // by definition of NonEmpty.contains

**Induction step: NonEmpty(z, l ,r) where y < z < x**
(NonEmpty(z, l ,r) incl y )contains x // to show = NonEmpty(z, l ,r) contains x
= NonEmpty(z, l incl y, r) contains x // by definition of NonEmpty.incl
= r contains x //by definition of NonEmpty.contains
= NonEmpty(z, l ,r) contains x // by definition of NonEmpty.contains

case **x < z**
**Induction step: NonEmpty(z, l ,r) where y < x < z**
(NonEmpty(z, l ,r) incl y )contains x // to show = NonEmpty(z, l ,r) contains x 
= NonEmpty(z, l incl y, r) contains x
= (l incl y) contains x
= l contains x
= NonEmpty(z, l, r) contains x

## Streams
Avoid computing tail of a sequence until it is needed for the evaluation result
streams are similar to lists, but their tail is evaluated only on demand

### Defining streams
`val xs = Stream.cons(1, Stream.cons(2, Stream.empty))`
 or
 Stream(1, 2, 3)
 
Stream Range
```
 def streamRange(lo: Int, hi: Int): Stream[Int] = 
  if(lo >= hi) Stream.empty
  else Stream.cons(lo, streamRange(lo + 1, hi))
```
Streams support all methods of a List

### Stream Cons operator
x :: xs always produces a list, never a stream
x #:xs == Stream.cons(x, xs)

`#:` can be used in expressions as well as patterns

### Implementation of Streams
concrete implementations of streams are defined in the Stream companion object
since tail is a call by name parameter in
 def cons[T](hd: T, tl: => Stream[T]) = new Stream[T] {
 ...
 }
 when we construct cons for stream, the tail is not evaluated, it wil be evaluated when first time someone de-references the tail
 i.e. someone calls the tail method, the tail parameter will be de-referenced.
 
 we just evaluate the first element, the take method would not do anything special it would return a stream with only 1 element
 but when we convert the stream to a list, then ofcourse we need to force it because the list cannot be left unevaluated,
 by the time we do that we create a list with 3 elements, and to produce that list we need to go down 3 elements
 
## Lazy Evaluation
Instead of evaluating tail everytime, we store the result of the first evaluation of tail and re-use the stored result instead of recomputing tail
Scala uses strict evaluation by default, but allows lazy evaluation of value definitions with the lazy val form:

`lazy val x = expr`

eg:
 
```
scala> def expr = {
     | val x = {print("x"); 1}
     | lazy val y = { print("y"); 2 }
     | def z = { print("z"); 3 }
     | z + y + x + z + y + x
     | }
expr: Int
```
the above example prints out `xzyz`, because in order to evaluate `expr` we will need to evaluate `x`, `y`, and `z` individually.
we see `x` is a val so it gets immediately evaluated, hence it prints out `x`, when we reach the last line, we see `z` is being used, it gets evaluated and prints a `z`
now, we again see `x` which is not re-evaluated, we see `y` which is now evaluated, we again see `x` which is not evaluated, and `z` which is evaluated again and prints a `z`
and lastly `y` and `x` being evaluated are not re-evaluated.

## Computing with Infinite Sequences
A stream of natural numbers can be considered to be a infinite stream
def from(n: Int): Stream[Int] = n #:: from(n + 1)

Eg: The Sieve of Eratosthenes

- start with all integers from 2, the first prime number
- eliminate all multiples of 2
- the first element of the resulting list is, a prime number
- eliminate all multiples of 3
- iterate forever. At each step, the first number in the list is a prime number and we eliminate all its multiples

```
def sieve(s: Stream[Int]): Stream[Int] = 
 s.head #:: sieve(s.tail filter(_ % s.head != 0))
eg:
 val primes = sieve(from(2)) wil give us Stream[Int] = Stream(2, ?)
 primes.take(100).toList
```

Eg: Square Roots 
```
def sqrtStream(x: Double): Stream[Double] = {
 def improve(guess: Double) = (guess + x / guess) / 2
 lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
 guesses
}
```

# Week 3

## Functions and State

### Stateful Objects

An object has a state if its behavior is influenced by its history

### Implementation of State
Every form of mutable state is constructed from variables. A variable definition is written like a value definition, but the keyword used is a var
`eg: var x: String = "abc"`
whose value can be changed through an association
 
x = "def"

In practice objects with state are usually represented by objects that have some variable members

```
class BankAccount {
 private var balance = 0
 def deposit(amount: Int): Unit = {
  if (amount > 0) balance = balance + amount
 }
 def withdraw(amount: Int): Int = {
  if (0 < amount && amount <= balance) {
   balance = balance - amount
   balance
  } else throw new Error("Insufficient funds")
 }
}
```

## Identity and change

Assignment poses a new problem of deciding whether 2 expressions are "the same"
when we exclude an assignment we write:
val x = E; val y = E, where E is an arbitrary expression, then it is reasonable to assume x and y are the same, we cold have written as
val x = E; val y = x
this is called `Referential Transparency`

operational equivalence: suppose we have 2 definitions of x and y, x and y are operationally equivalent if no possible test can distinguish between them

## Loops

variables model all imperative programs, but what about loops?
eg:

```
def power(x: Double, exp: Int): Double = {
 var r = 1.0
 var i = exp
 while(i > 0) { r = r * x; i = i - 1}
 r
}
```

how to write a function implementing a repeat loop:
def REPEAT (command: => Unit)(condition: => Boolean) = {
 command
 if(condition) ()
 REPEAT(command)(condition)
}

### For-loops:
the classical for loop in java cannot be modeled simply by a higher-order function, the reason is that in a Java program,
for( int i = 1; i < 3; i = i + 1) { System.out.println(i + " "); }
the arguments of `for` contain the declaration of the variable `i`, which is visible in other arguments and in the body.

For-loops translate similarly to for-expressions, but using the `foreach` combinator instead of `map` and `flatMap`
`foreach` is defined on collections of type T as follows:
```
 def foreach(f: T => Unit): Unit =
 //apply f to each element of the collection
 ```
 
## Discrete Event Simulation
we define wires as a class Wire.
val a, b, c = new Wire

we define Gates as follows:
abstract class Gates extends Simulation {
def inverter(input: Wire, output: Wire): Unit
def andGate(a1: Wire, a2: Wire, output: Wire): Unit
def orGate(a1: Wire, a2: Wire, output: Wire): Unit
}

## API usage for discrete event simulation
Simulation trait
a concrete simulation happens inside an object that inherits from the trait Simulation
```
trait Simulation {
 def currentTime: Int = ???
 def afterDelay(delay: Int)(block: => Unit): Unit = ???
 def run(): Unit = ??? 
}
```

The Wire class:
A wire must support 3 basic operations:
`getSignal: Boolean`, returns the current value of the signal transported by the wire
`setSignal(sig: Boolean): Unit`, modifies the value of the signal transported by the wire
`addAction(a: Action): Unit`

```
class Wire {
 private var sigVal = false
 private var actions: List[Action] = List()
 def getSignal: Boolean = false
 def setSignal(s: Boolean): Unit = {
  if (s != sigVal) {
   sigVal = s
   actions foreach(_ ())
  }
 }
 def addAction(a: Action): Unit = {
  actions = a :: action
  a()
 }
}
```

the way inverter will work is as follows:
we implement the inverter by installing an action on its input wire. This action produces the inverse of the input signal on the output wire.
The change must be effective after a delay of IntervalDelay units of simulated time.
```
def inverter(input: Wire, output: Wire): Unit = {
 def invertAction(): Unit = {
  val inputSig = input.getSignal
  afterDelay (InverterDelay) { output setSignal !inputSig }
 }
 input addAction invertAction
}
```

similarly we implement ANDGate and ORGate as follows:

```
def andGate(input1: Wire, input2: Wire, output: Wire): Unit = {
 def andAction(): Unit = {
  val inputSig1 = input1.getSignal
  val inputSig2 = input2.getSignal
  afterDelay (AndDelay) { output setSignal (inputSig1 & inputSig2) }
 }
 
 input1 addAction andAction
 input2 addAction andAction
 
}
```

```
def orGate(input1: Wire, input2: Wire, output: Wire): Unit = {
 def orAction(): Unit = {
  val inputSig1 = input1.getSignal
  val inputSig2 = input2.getSignal
  afterDelay (AndDelay) { output setSignal (inputSig1 |inputSig2) }
 }
 
 input1 addAction orAction
 input2 addAction orAction
 
}
```

The idea is to keep an `agenda` of actions to be performed, where an `agenda` is the list of (simulated) events. 
Each `event` consists of an action and the time when it must be 
produced
The agenda list is sorted in such a way so that the actions to be performed first are in the beginning.

```
abstract class Simulation {
 type Action = () => Unit
 case Event(time: Int, action: Action)
 private type Agenda = List[Event]
 private var agenda: Agenda = List()
 private var curTime = 0
 def currentTime: Int = curTime
 
 //insert a task into the agenda list at the right position
 def afterDelay(delay: Int)(block: => Unit): Unit = {
  val item = Event(curTime + delay, () => Unit)
  agenda = insert(agenda, item)
 }

 def run(): Unit = {
  afterDelay(0) {
   println(s"Started at $curTime"))
  }
  loop()
 }
 
 private def insert(ag: List[Event], item: Event): List[Event] = ag match {
 case first :: rest if first.time <= item.time => first :: insert(rest, item)
 case _ => item :: ag
 }

 private def loop(): Unit = agenda match {
  case first :: rest =>
   agenda = rest
   curtime = first.time
   first.action()
   loop()
  case Nil => 
 }
 
}
```

before launching the simulation we need a way to examine the changes of the signals on the wires.
```
 def probe(name: String, wire: Wire): Unit = {
  def probeAction(): Unit = {
   println(s"$name $curTime value= ${wire.getSignal}")
  }
  wire addAction probeAction
 }
```

```
abstract class Gates extends Simulation {
 def InverterDelay: Int
 def AndGateDelay: Int
 def OrGateDelay: Int
 
 class Wire {
  private var sigVal = false
  private var actions: List[Action] = List()
  def getSignal: Boolean = false
  def setSignal(s: Boolean): Unit = {
   if (s != sigVal) {
    sigVal = s
    actions foreach(_ ())
   }
  }
  def addAction(a: Action): Unit = {
   actions = a :: action
   a()
  }
 }
 
 def andGate(input1: Wire, input2: Wire, output: Wire): Unit = {
  def andAction(): Unit = {
   val inputSig1 = input1.getSignal
   val inputSig2 = input2.getSignal
   afterDelay (AndDelay) { output setSignal (inputSig1 & inputSig2) }
  }
  
  input1 addAction andAction
  input2 addAction andAction
 }

 def orGate(input1: Wire, input2: Wire, output: Wire): Unit = {
  def orAction(): Unit = {
   val inputSig1 = input1.getSignal
   val inputSig2 = input2.getSignal
   afterDelay (AndDelay) { output setSignal (inputSig1 |inputSig2) }
  }
  
  input1 addAction orAction
  input2 addAction orAction
 }
 
 def inverter(input: Wire, output: Wire): Unit = {
  def invertAction(): Unit = {
   val inputSig = input.getSignal
   afterDelay (InverterDelay) { output setSignal !inputSig }
  }
  input addAction invertAction
 }
 
  def probe(name: String, wire: Wire): Unit = {
   def probeAction(): Unit = {
    println(s"$name $curTime value= ${wire.getSignal}")
   }
   wire addAction probeAction
  }
}

trait Parameters {
 def InverterDelay = 2
 def AndGateDelay = 3
 def OrGateDelay = 5
}
```

### Summary
State and assignment make our mental model of computation more complicated
we loose referential transparency
assignments allow us to formulate certain programs in an elegant way

# Week 4

## Imperative event handling: The Observer Pattern
Observer Pattern, the Good

- decouples views from state
- allows to having varying number of views of a given state
- simple to setup

The Bad,

- forces imperative style, since handlers are Unit-typed
- many moving parts that need to be co-ordinated
- concurrency makes things more complicated
- views are still tightly bound to one state; view updates happens immediately

## Functional Reactive Programming (FRP)
Reactive programming is about reacting to sequence of events that happen in time
a functional view would be that we aggregate an event sequence into a signal

- A signal is a value that changes over time
- represented as a function from time to the value domain
- instead of propagating updates to a mutable state one by one we define new signal in terms of existing ones

Generally an indexed assignment like `f(E1, ..., En) = E` is translated to `f.update(E1, ..., En, E)`. 
This also works if n = 0: `f() = E` is shorthand for `f.update(E)`

Signals of type `Var` look a bit like mutable variables, where `sig()` is dereferencing, and `sig() = newValue` is update

```
class BankAccount {
 val balance = Var(0)
 def deposit(amount: Int): Unit = {
  if (amount > 0) balance() = balance() + amount //this is a cyclic signal definition which will not work, since over time we say
  //the balance a function over time is the sum of the balance at that time + amount
 }
 def withdraw(amount: Int): Int = {
  if (0 < amount && amount <= balance()) {
   balance() - amount
  } else throw new Error("Insufficient funds")
 }
}
```
 
much better solution
```
class BankAccount {
 val balance = Var(0)
 def deposit(amount: Int): Unit = { 
  if (amount > 0) {
   //take the current value of balance() b
   val b = balance()
   //then define a new balance() to be that value + amount
   balance() = b + amount
  }
 }
 def withdraw(amount: Int): Int = {
  if (0 < amount && amount <= balance()) {
   val b = balance()
   balance() = b - amount
  } else throw new Error("Insufficient funds")
 }
}
```

## A simple FRP implementation
Thread local state means that each thread accesses a separate copy of a variable, it is supported in scala through class scala.util.DynamicVariable
disadvantages:

- its imperative nature often produces hidden dependencies which are hard to manage
- its implementation on the JDK involves a global hash table lookup, which can be a performance problem
- does not play well in situations where threads are multiplexed between several tasks

a cleaner solution involves making use of implicit parameters:

- instead of maintaining a thread-local variable, pass its current value into a signal expression as an implicit parameter
- this is purely functional but requires more boilerplate code

## Latency as an effect
### Four essential effects in programming
 
             One            Many
Synchronous  T/Try[T]       Iterable[T]
Asynchronous Future[T]      Observable[T]

Future[T] a monad that handles exceptions and latency
Futures asynchronously notify consumers

```
import scala.concurrent.ExecutionContext.Implicits.global
trait Future[T] {
 def onComplete(callback: Try[T] => Unit)
}
```
callback needs to use pattern matching

### Futures alternative designs

```
trait Future[T] {
//2 callbacks, 1 for success and 1 for failure
 def onComplete(success: T => Unit, failed: Throwable => Unit): Unit
 def onComplete(callback: Observer[T]): Unit
}

trait Observer[T] {
 def onNext(value: T): Unit
 def onError(error: Throwable): Unit
}
An object is a closure with multiple methods, and a closure is an object with a single method
```

Example:

```
trait Socket {
 def readFromMemory(): Future[Array[Byte]]
 def sendToEurope(packet: Array[Byte]): Future[Array[Byte]]
}

val socket = Socket()
val packet: Future[Array[Byte]] = socket.readFromMemory()
val confirmation: Future[Array[Byte]] = {
 packet.onComplete {
  case Success(p) => socket.endToEurope(p)
  case Failure(t) => ...//we get a type from Try[T] => Unit, what we need is a Future[Array[Byte]]
 }
}

another way which makes the use of callback ugly is:
packet.onComplete {
 case Success(p) => {
  val confirmation: Future[Array[Byte]] = socket.sendToEurope()
 }
 case Failure(t) => ...
}
```

### Construction of Futures
Starts a asynchronous computation and returns a future object to which you can subscribe to be notified when the future completes

```
object Future {
 def apply(body: => T)(implicit context: ExecutionContext): Future[T]
}
```

The behavior of the future is that the callback is called atmost once because the computation might never terminate, but when it terminates it is called exactly once with the value
and if computation is already terminated and you are adding a new callback that callback will be called immediately. 
```
import scala.concurrent.ExecutionContext.Implicits.global
import akka.serializer._

val memory = Queue[EmailMessage](
 EmailMessage(from = "foo", to = "bar"),
 EmailMessage(from = "foo1", to = "bar")
)

def readFromMemory(): Future[Array[Byte]] = Future {
 val email = memory.dequeue
 val serializer = serialization.findSerializerFor(email)
 serializer.toBinary(email)
}
```

NOTE: When a callback is put to the function `readFromMemory`, the code will in that body will only be executed only once, so we have a code with a side-effect but with every callback 
the side-effect will happen only once.

### Combinators on Futures

rewrite `readFromMemory` using `flatMap`
```
val socket = Socket()
val packet: Future[Array[Byte]] = socket.readFromMemory()

val confirmation: Future[Array[Byte]] = 
 packet.flatMap(p => socket.sendToEurope(p))
```

NOTE: When you are using asynchronous code you should `never ever block`, except when you are demoing, or debugging etc.
```
val socket = Socket()
val packet: Future[Array[Byte]] = socket.readFromMemory()

val confirmation: Future[Array[Byte]] = 
 packet.flatMap(p => socket.sendToEurope(p))

val c = Await.result(confirmation, 2 seconds)
println(c.toText)
```

### Composing Futures
we can use for-comprehensions on a Future
```
val socket = Socket()
val confirmation: Future[Array[Byte]] = for {
 packet: Future[Array[Byte]] <- socket.readFromMemory()
 confirmation <- socket.sendToSafe(packet)
 } yield confirmation
``` 

### Retrying to send
```
def retry(noTimes: Int)(block: => Future[T]): Future[T] = {
 //retry successfully completing block at most noTimes
 //and give up after that
}
```

### Implementation of flatMap on Future
implement flatMap on Future and implement flatMap in terms of onComplete
```
trait Future[T] { self =>
  ...
 def flatMap[S](f: T => Future[S]): Future[S] = 
  new Future[S] {
   def onComplete(callback: Try[T] => Unit) =
    self onComplete {
     case Success(x) => f(x).onComplete(callback) //f(x) gives us Future[S], but we need the return type of Unit, we hence call onComplete
     case Failure(e) => callback(Failure(e))
    }
  }
}
```

we make use of `foldLeft` or `foldRight` to solve the retry function
```
def retry(noTimes: Int)(block: => Future[T]): Future[T] = {
 val ns = (1 to noTimes).toList
 val attempts = ns.map(_ => () => block)
 val failed = Future.failed(new Exception("boo"))
 val result = attempts.foldLeft(failed)((a, block) => a recoverWith {block()})
 result
}

Example:
retry(3){block} unfolds as follows:
((failed recoverWith{block1()})
  recoverWith{block2()})
   recoverWith{block3()}
```

using foldRight
```
def retry(noTimes: Int)(block: => Future[T]): Future[T] = {
 val ns = (1 to noTimes).toList
 val attempts = ns.map(_ => () => block)
 val failed = Future.failed(new Exception("boo"))
 val result = attempts.foldRight(() => failed)( (block, a) => () => { block() fallbackTo { a() } } )
 result
}

Example:
retry(3){ block } () unfolds as follows:
block1 fallbackTo { block2() fallbackTo { block3() fallbackTo { failed } } }
```

# Conclusion

- lazy evaluation and infinite data structures
- distinction between computation and values
- monads to abstract over properties of computation: randomness, delays, effects
- running computations at some later time
how to encapsulate mutations
- laziness
- FRP
- monads
