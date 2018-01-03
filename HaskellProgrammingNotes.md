### 11/15/2017 (Wednesday)

**Pages 194.5-200.5 (6 pages)**

#### Show

* The `Show` typeclass is used for displaying datatypes as human-readable strings.
* `Show` is _not_ for serialization; it is specifically for human-readable string representations.
* Remember, Haskell is a pure functional language. It's _pure_ because it can be expressed entirely as a lambda calculus. It's _functional_ because it relies on mathematical functions to get things done.
* Haskell separates pure computations and effectful computations.
* Haskell also preserves its purity in the way that it handles effects. It is one of the only FP languages that does not extend its lambda calculus; e.g., even with effects, it is still a pure language.
* The `main` function for a program _must_ be of type `IO ()`.
* The type `()` is an _empty tuple_, and it is referred to as **unit**. 
* The `IO` wrapper is used to denote values which perform side effects. You can think of an IO value as a _means of producing_ a value. For example, a function with return type `IO String` is a "formula" or "means of producing" a string by performing some side effects, such as reading from the user input.
* The `print` function has the following type signature: `Show a => a -> IO ()`. We can see that the value to print must have an instance of `Show`, and that printing is an effectful operation.
* Bottom line: _you cannot print values without a Show instance_.
* Show is a derivable typeclass. Example: `data Mood = Blah deriving Show`.

#### Read

* The Read typeclass exposes operations for converting Strings into different types. It is not a serialization format.
* This typeclass is **not safe!** You cannot possibly know that a given string will be parsable into a given data type. And when it can't be parsed? Runtime exception city!!!!
* The `read` function is a _partial function_. It does not provide a result for every possible input! Bad.

#### Instances are dispatched by type

* Typeclasses define a set of functions and/or values
* Types have _instances_ of typeclasses
* A typeclass **instance** specifies the way a particular type uses the functions and exposes the values defined by the typeclass. 

### 11/16/2017 (Thursday)

**Pages 200.5-205 (4.5 pages)**

* Haskell figures out what instance of a typeclass to use for a typeclass function invocation based on the types provided in the function. Haskell _dispatches by type_.
* Haskell can't guess the type of a constant defined in a typeclass unless you assert it, like we've done with `minBound` before: `minBound :: Int`.
* In Haskell, **dispatching on a typeclass instance** means specifying the typeclass instance to use for a given expression. This is something the language does automatically, not something you do (though you could indirectly trigger it by asserting a type, as shown above).
* Typeclasses should follow rules and laws.
* **Concrete types imply all the typeclasses they provide** (quote). This means that when you provide a concrete type in a function's type signature, that type will have access to all the operations defined in the typeclasses it instantiates.

### 11/17/2017 (Friday)

**Pages 205-209.5 (4.5 pages)**

* A concrete type has a typeclass or it doesn't; addingg a constraint to it means nothing!
* Concrete types always and only imply the typeclasses they implement.
* It can be wise to use the most polymorphic type possible for your function, because it helps to describe what the function is doing and to help you avoid making mistakes.

**Chapter 6 Exercises (pp.206-211)**

**Multiple choice**

1. c) makes equality tests possible
1. b) is a subclass of Eq
1. a) `Ord a => a -> a -> Bool`
1. c) the type of x is a tuple
1. a) Int and Integer numbers

**Does it typecheck?**

1. No. `Person` does not have a `Show` instance. (correct)
2. No. `Mood` does not have an `Eq` instance. (correct)
3. 
    1. `Mood` values
    1. It will throw an error, because you cannot compare a `Num` with a `Mood`
    1. It will throw an error, because `Mood` doesn't implement the `Ord` typeclass
4. Yes... (correct)

**Given a datatype declaration, what can we do?**

1. Will not typecheck, because the values "chases" and True need to be wrapped in `Rocks` and `Yeah`, respectively. (correct)
2. Will typecheck. (correct)
3. Will typecheck. Papu has an Eq instance. (correct)
4. Will not typecheck. Papu does not have an Ord instance. (correct)

### 11/18/2017 (Saturday)

**Pages 209.5-216 (6.5 pages)**

**Match the types**

1. No; the parametrically polymorphic type variable `a` is too general to contain the number `1`. (correct)
2. No; `Num a` is too general to contain a decimal number. (correct)
3. Yes; `Fractional a` can contain decimal numbers. (correct)
4. Yes; Float implements RealFrac, and RealFrac requires Fractional. (correct)
5. This will work, however, it's kind of silly. (correct)
6. This will also work, and it is also kind of silly. (correct)
7. No; `Int` cannot be coerced into `a`. (correct)
8. No; the concrete type `Int` cannot be replaced with the general type `Num a => a`. (correct)
9. Yes; you can specialize `Ord a` to `Int` since Int implements the Ord typeclass. (correct)
10. Yes; this will work because the function's behavior relies on the presence of the `Ord` typeclass, not on the concrete type `Char`. (correct)
11. No; `mySort` requires a list of Char, so `signifier`, which calls it, also requires a list of Char. (correct)

**Type-Kwon-Do Two: Electric Typealoo**

1. `chk f a b = b == f a` (correct)
2. `arith f i a = f a * f a` (correct)

**Definitions**

* **Typeclass inheritance** is when one typeclass requires another. The required typeclass is a _superclass_ of the typeclass requiring it. This means that when you want to write an instance of a typeclass with one or more superclasses, you must first ensure that the type you are writing the instance for has instances of those superclasses.
* **Effects** are _observable_ actions a program takes outside of computing values. If we can observe a function doing something to the outside world other than just computing and returning a value, then we can say that function has an _effect_.
* The `IO` type is used for values which may produce effects.
* A typeclass **instance** is a specification of how a typeclass works for a particular type. Instances are _unique combinations of types and typeclasses_. You cannot define two instances for the same typeclass for the same type.
* It is possible to _derive instances_ for certain typeclasses, such as `Eq`, `Enum`, `Ord`, and `Show`, so that an instance is automaticallly generated based on the datatype definition and you, the programmer, don't have to write one.

### 11/19/2017 (Sunday)

**Pages 216-221.5 (5.5 pages)**

### Chapter 7: More Functional Patters

* Functions in Haskell take one argument and return one result.
* When you apply an argument to a function, its value is bound to the corresponding parameter in the head of the function and that value is replaced in the body (when the function is evaluated)
* **First-class** values are those which can be used as arguments to a function.
* In Haskell, **all values are first-class**. Therefore, functions are also first-class (you can pass them as arguments to other functions).
* You may define function parameters by placing them between the function name (which appears on the leftmost margin) and the equals sign in a function definition. Example: `func param1 param2 = param1`
* Functions are values which accept a parameter; values which do not define a parameter, such as `myInt = 5` are just regular values (_not_ functions).
* Polymorphic variables whose letters vary in a type signature can be _but are not required to be_ different types. For example, in `const :: a -> b -> a`, `a` and `b` can be different types, but they don't have to be.
* It is possible to bind variables through function application and in let expressions:
    * Function application: given the function `addOne x = x + 1`, you can _bind_ `x` to `1` by applying 1 as an argument: `addOne 1`
    * `let` expression: 

    ```
    bindExp :: Integer -> String
    bindExp x =
      let y = 5 in
      "the integer was: " ++ show x ++ " and y was: " ++ show y
    ```
    The variable `y` is bound to the value `5` in the `let` expression.
* It is possible to **shadow** variables so that they cannot be accessed in a certain scope. Example: 
```
shadowThings :: Integer -> Integer
shadowThings x = 
  let y = 5 in
    let y = 2 in
      x + y
```
In this case, the binding of `y` to `5` is shadowed by the inner `let` expression, and therefore is not visible within that inner let expression's scope.
* **Haskell is lexically scoped.** Another term for lexical scoping is _static_ scoping.
* **Lexical scoping** means that variable value resolution is based on the location in the code that variable appears and what the _lexical context_ is.