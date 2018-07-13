# Haskell Programming Notes

### 10/18/2017 (Wednesday)
**Pages 2 - 5.5 (3.5 pages)**
#### Functional Programming
* Functional programming is a paradigm where functions map one input value to one output value, like mathematical functions.
* The set of _input_ values for a function is called the **domain**, the set of output values is called the **codomain**. The subset of output values the function actually uses is called the **image** or **range**.
  
  For example, the domain may be all positive integers, and the codomain may be all positive integers as well, but the image may only be {1,2,3}.
* **Referential transparency** means that functions *deterministically* map one input to one output. Given the same input, the function **_always_** returns the same output.
* Haskell is a _pure functional programming language_ because it is a lambda calculus.
#### Lambda Calculus
* The **lambda calculus** is a model of computation which formalizes which problems and classes of problems in computation are solvable.
* Lambda calculus consists of three components:
* * **Abstractions** - Functions
* * **Variables** - Names for values
* * **Expressions** - An expression can be an abstraction, a variable, or a combination of both.
* **Abstractions consist of two parts**: A **head** and a **body**, which are **separated by a period**:
  1. The _head_ consists of a 𝜆 (lambda) followed by a variable name. Example: **𝜆x**
  2. The _body_ is an expression. 
* **Lambdas abstractions are _anonymous functions_**. They have no names.
* **Arguments** are input values for lambda functions.

### 10/19/2017 (Thursday)
**Pages 5.5 - 12.5 (7 pages)**
#### Lambda Calculus
* Lambda abstractions are also called *functions* or just *lambdas*.
* **Bound variables** are those variables which occur in the head and body of a lambda abstraction. In `𝜆x.x`, `x` is a bound variable.
* **Free variables** are those variables which are _not bound_ in the head of a lambda abstraction. In `𝜆x.xy`, `y` is a free variable.
* **Alpha equivalence** occurs when two lambda abstractions are logically identical. For example, `𝜆x.x` is alpha equivalent to `𝜆d.d`. Alpha equivalence *does not apply* to free variables, e.g., `𝜆x.a` is _not_ alpha equivalent to `𝜆x.z` because we don't know anything about `a` or `z`.
* **Application** is the process of passing an argument to a lambda function. For example, in `(𝜆x.x) 5`, `5` is applied to `𝜆x.x` (the identity function).
* **Beta reduction** is the process of replacing bound variables with arguments until there are no heads or arguments remaining. Once a variable is replaced in the body, the head in which it was bound is discarded. 

  **Example**:
  1. `(𝜆x.x) 5` is `𝜆x.x` applied to `5`
  2. `(𝜆[x = 5].x`; assign `x` to `5`
  3. `5`; throw away the head and replace the `x` in the body with `5`.
  
  **Note that the purpose of the head is to bind variables**. This is important when free variables are present; without the head, you couldn't distinguish between free and bound variables!
  
* Lambdas may be applied to other lambdas, for example: `(𝜆x.x)(𝜆y.y)`.
* In the lambda calculus, **application** (reduction) **is left-associative**. This means that `(𝜆x.x)(𝜆y.y)(5)` is equivalent to `((𝜆x.x)(𝜆y.y))(5)`. (This expression evaluates to `5`.) 
* **Lambdas may only bind one variable and accept one argument**. Lambdas with multiple bound variables, like `𝜆xy.xy` are actually nested lambdas which each bind one variable (in this case, `𝜆x.𝜆y.xy`). This is called **currying**. 

### 10/20/2017 (Friday)
**Pages 12.5 - 16.5 (4 pages)**

**Exercises on page 13**:
1. `𝜆𝑥𝑦.𝑥z` ~= `𝜆mn.mz` (b)
2. `𝜆𝑥𝑦.xxy` ~= `𝜆a.(𝜆b.aab)` (c)
3. `𝜆𝑥𝑦z.zx` ~= `𝜆tos.st` (b)

#### Lambda Calculus

* **Beta normal form** is when a lambda expression cannot be reduced any further via beta reduction. In programming, this is a fully executed program.
* There is a difference between a fully applied function and a fully _evaluated_ function. For example, `(𝜆𝑥.x) 5` is fully _applied_, but it is not in beta normal form because it is not fully _evaluated_. In beta normal form, the expression becomes `5`.
* **Combinators** are lambda abstractions with _no free variables_. For example, `𝜆𝑥𝑦.𝑥y` is a combinator, while `𝜆𝑥𝑦.𝑥z` is _not_, because `z` is a free variable. Combinators are important because they only combine their bound variables.
* It is possible for lambda expressions to **diverge**, which means that they can never be reduced to beta normal form.
  
  For example: `(𝜆𝑥.𝑥x)(𝜆𝑥.𝑥x)` diverges because its reduction results in the original expression. (This particular expression is called _omega_).
* Divergence is important in programming because programs that diverge never produce an answer or result; they never halt.


### 10/21/2017 (Saturday)
**Pages 16.5 - 24 (7.5 pages)**

* Functional programming is about expressions. Expressions may contain variables/constant values, functions, or combinations of expressions.
* Given the same input, a function always produces the same output.
* Haskell is pure because it is a lambda calculus. It has a lot of syntactic sugar to make it nicer for the programmer, but it is still a lambda calculus.
* Functions take exactly _one_ argument and return _one_ result.
* Functions are expressions that have a _head_ and a _body_ and can be applied to arguments to be evaluated and return a value.
* > _Lambda calculus_ is a formal system for expressing programs in terms of abstraction and application.
* Haskell is not evaluated in _normal order_. It is evaluated in _call-by-need_ order. This will be explained later.

**Chapter 1.11 Exercises (p. 17)**

Determine if the following are combinators.

(Note to self: Remember that combinators are lambdas with _no free variables_).
1. Yes
2. No
3. Yes
4. Yes
5. No

Determine if each of the following can be reduced to beta normal form.

1. `𝜆x.xxx` does not diverge and is in normal form (no arguments to apply).
2. This is a divergent lambda expression called _omega_.
3. This expression does not diverge and reduces to `zzz`

Beta reduce each of the following expressions to normal form.

**Evaluation takes place in normal order**. Normal order means that the outermost and left-most terms are evaluated first. If there is a nested application and an outer application, then the outer application should be evaluated first, even if it is rightmost.

(Note to self: Remember that function application is _left associative_.)

I did the following exercises on paper. The answers are shown here.
1. `z`
2. `bb`
3. `qq`
4. `yy`
5. `yy`
6. `aac`
7. 𝜆z1.za (REMEMBER, you may only rename BOUND variables, _not_ free variables!)

### 10/22/2017 (Sunday)
**Pages 25 - 31.5 (6.5 pages)**

#### Chapter 2: Hello Haskell!

#### Stack and the REPL

* **Stack** is a cross-platform build and project management tool for Haskell.
* There are two primary ways to interact with Haskell:
  1. Via the REPL (GHCi)
  2. By creating a module in a file and loading it with GHCi
* You can run the REPL with either of the following commands: `stack ghci` or `ghci`.
* To exit GHCi, type `:quit`.
* **Prelude** is a library of standard functions. It is automatically imported in the REPL. It is included in Haskell's `base` package.
* Special commands in the REPL begin with a colon (:).
* Developing in Haskell usually follows these steps:
  1. Edit code in a source file
  2. Load source module in REPL and interact with and test it
  3. Repeat
* You can use the `:load` command in the REPL to load all the functions from a source file into the REPL.
* To reset your REPL back to the default state (with no loaded modules except Prelude), use the `:module` command.

#### Haskell

* All Haskell consists of two fundamental pieces:
  1. **Expressions** - values, function applications, or combinations of expressions. Expressions always return a value.
      * Examples: `1`, `1 + (3 * 2)`, `f 1 2 3`
  2. **Declarations** - allow you to name expressions so they can be reused without copy-pasting code.
* `::` is for writing down type signatures. It can be read as "has the type". For example: `num :: Number` is read as "num has the type Number."
* Expressions are in **normal form** when they can be reduced no further.
* Reducible expressions are called **redexes**.
* **Functions are expressions**.
* As in lambda calculus, so in Haskell. Functions accept **one** argument and return **one** result. Functions which accept multiple arguments are actually _curried_, which means they're a series of nested single-argument functions.
* Functions allow programmers to abstract out pieces of common functionality so that they may be re-used in multiple places.
* Functions in Haskell can be used anywhere a regular value can be used.

### 10/23/2017 (Monday)
**Pages 31.5 - 39 (7.5 pages)**

#### Haskell

* Function definitions/declarations look like this:
  
  `<function_name> <formal_param_1> <formal_param_2> ... = <expression>`

  There can be N formal parameters. They are all separated by spaces. Here is an example function:

  `add num1 num2 = num1 + num2`
* In versions of GHCi < 8.0.1, you must use `let` to define a function, e.g., `let <function_defintion>`
* Function names must begin with a lowercase letter.
* _Formal parameters_ are the named parameters in the function definition, _arguments_ (or _actual parameters_) are the values you pass in to the function when you apply it.
* Function parameters represent the _head_ in the lambda representing the function and the function body expression is the lambda _body_. (Remember that currying applies to functions.)
* Haskell uses **non-strict/lazy evaluation**. This means that terms are only evaluated when needed (i.e., when other terms refer to them and force them to be evaluated).
* Values are **irreducible**, meaning they cannot be reduced any further.
* Functions can be reduced by applying arguments.
* **Canonical** form is the same as **normal** form (an expression is fully reduced).
* **Haskell evaluates to weak head normal form (WHNF)**. This means that expressions which need not be reduced aren't.

**Exercises: Comprehension Check** (p.34)

1. Make the following functions work in the REPL.
  *  `half x = x / 2` becomes `let half x = x / 2` in the REPL.
  * `square x = x * x` becomes `let square x = x * x` in the REPL.
2. Write a function ... `pify x = x * 3.14`
3. Write a function ... `pify' = (*) pi`

#### Infix Operators

* By default, functions in Haskell use **prefix syntax**. This means that, when applying the function to arguments, the function name comes first, then the arguments. Example: `add 3 4`.
* **Infix syntax** or infix style is a way of applying functions so that the function name appears in between the arguments. For example: `3 + 4` (`+` is an infix operator).
* **Operators** are functions which use infix style.
* You may use a function in infix style by surrounding its name in tics. For example: ```3 `add` 4```.
* You may use an infix operator in prefix style by wrapping it in parentheses. For example: `(+) 3 4`.
* Functions with alphanumeric names are prefix by default. Functions with symbolic names are infix by default.
* Operators have a precedence from 0-9. Operators with higher precedence are applied first.
* `infixl <number> <symbol>` means there is a `<symbol>` operator with a `<number>` precedence and which associates to the left (`l` on the end). `infixr ...` means it associates to the right.
* Associativity means that changing the parentheses around operands will not affect the result of an expression.
* Commutativity means that changing the _order_ of operands will not affect the result of an expression.
* Associativity in operator definitions is important for non-associative operators.

**Exercises: Parentheses and Association** (p.38)

1. `8 + 7 * 9` vs `(8 + 7) * 9`. Yes, the parentheses change the results of the expression. Multiplication has higher precedence than addition.
2. `perimeter x y = (x * 2) + (y * 2)` vs `perimeter x y = x * 2 + y * 2`. No, the parentheses don't change the results of the expression.
3. `f x = x / 2 + 9` vs `f x = x / (2 + 9)`. Yes, the parentheses change the results of the expression.

#### Declaring values

* Order of declaration **matters in the REPL**
* Order of declaration **does not matter in source files** because they are loded all at once.

### 10/24/2017 (Tuesday)

**Pages 40-45.5 (5.5 pages)**

* You can declare a **module** in a file by placing it at the top:
  
  ```module MyModule where```
* Module names are capitalized.
* Indentation matters in Haskell. It is used as a syntactic boundary.
* Terms in a group must have the same level of indentation. They must line up. For example:
  ```
  bam x =
    let y = 5
        z = 4 * x
    in y * z
  ```
* You can break up expressions across multiple lines, but if you do so, you must indent the piece(s) on the next line(s) by at least a space. Preferably, you should preserve the indentation of the term on the first line so the terms line up.
* All the declarations in a file should be at the leftmost margin to avoid compile errors.
* The compiler expects all declarations in a file to have the same _level of indentation_. Placing them on the leftmost margin is the simplest and recommended way of ensuring this.
* Line comments begin with `--`
* Import modules into ghci with either `ghci <filename>` or `gchi` then `:load <filename>` when in the directory of the file.


### 10/25/2017 (Wednesday)

**Pages 45.5-60 (14.5 pages)**

**Exercises: Heal the Sick**

Fix the mistakes in the code samples.

I will show the fixed versions.

1. `let area x = 3.14 * (x * x)`
2. `let double x = x * 2`
3. 
  ```
    x = 7
    y = 10
    f = x + y
  ```

#### Arithmetic

* `/` is fractional division
* `div` is _integral_ division which rounds down
* `quot` is integral division which rounds towards zero
* `rem` is remainder after division
* `mod` is remainder after modular division
* `quot` is to `rem` as `div` is to `mod`, per these laws:
  * `(quot x y)*y + (rem x y) == x`
  * `(div x y)*y + (mod x y) == x`
* `rem 10 -4 == 2` WHEREAS `mod 10 -4 == -2`
* **Modular arithmetic** is a system which allows for integers which "wrap around" at a certain value.
* A good example is a clock. In a clock, you do arithmetic _modulo 12_, which means that twelve is equal to both 12 and 0.
* Remember, a smaller integer mod a larger integer will always evaluate to the value of the smaller integer. This is because the larger number goes into the smaller integer **zero** times, with the smaller integer left over as a remainder.
* `mod` and `rem` give the same result when both numbers are _positive_, but not when one or both is negative.

**Bottom line**:
* `rem` will have the same sign as the _dividend_ (the number being divided)
* `mod` will have the same sign as the _divisor_ (the number the dividend is being divided by)

---
* You will often have to wrap negative numbers in parentheses when using them in arithmetic expressions. For example, `3 + (-6)` works but `3 + -6` does not work. The reason for this is that without the parenthetical wrapping, GHC thinks that `-` is referring to the subtraction function. Negating numbers with `-` is actually syntactic sugar; (-6) will actually get converted to `(negate 6)`. The compiler infers this from the context.
* The dollar sign ($) is a way of delaying function application so that you can remove parenthesis. It is `infixr 0`. For example, `(*3) (4 + 4)` is equivalent to `(*3) $ 4 + 4`. Also, `(*3) ((+3) (4 - 3))` is equivalent to `(*3) $ (+3) $ 4 - 3`.
* **In order to use an _infix_ operator as a _prefix_ operator, you must wrap the operator in parentheses** (e.g., `(+)`).
* **Sectioning** is when you partially apply an infix operator using prefix notation. For example, `(+3)` is `(+)` partially applied to `3`.
  * Operand position matters: `(/ 3) 6` is not equal to `(3 /) 6`.
  * **WARNING**: You cannot section with subtraction like you can with division, multiplication, and addition. This `(-1) 1` won't work, because GHC interprets `(-1)` as being "negative one" and not the subtraction function partially applied to 1. You _can_ place the operand on the left, though: `(1-) 1` works. To get around the negation problem, use `(subtract 1) 1`. 

#### Let and Where

* The two keywords `let` and `where` allow you to define variables to be used in some expression. The difference is that `let` is an _expression_, while `where` is a _declaration_ that is bound to some syntactic construct.
* The `:load` command in GHCi can only load ONE module at once! All other modules are unloaded.
* **Scope** is the area of code where the binding of a variable applies.
* **INDENTATION**: You must indent lines after the first line of a declaration or expression further in than the first line. In the case of `let` and `where`, if you wish to place a declaration on the first line, you must indent all subsequent lines so they line up exactly with the first declaration. This is because Haskell counts all characters to the left of the first declaration as whitespace, even if they're not. https://en.wikibooks.org/wiki/Haskell/Indentation

**Exercises: A Head Code (pp.58-60)**

Figure out what each expression returns, then verify in GHCi. 

I will show what I think the return values are (answers with ♫ are correct).

1. `5` ♫
2. `25` ♫
3. `30` ♫
4. `6` ♫

Rewrite the following expressions with where clauses: 

(expressions and answers omitted; see book and haskell-programming directory)

### 10/26/2017 (Thursday)

**Pages 60-67 (7 pages)**

**Chapter 2 Exercises (pp.60-63.5)**

Parenthesize the expressions so they are more explicit without changing their result.

1. `2 + (2 * 3) - 1` ♫
2. `(^) 10 (1 + 1)` ♫
3. `(2^2) * (4^5) + 1` ♫

Which of the following will return the same result when evaluated?

1. Same ♫
2. Same ♫
3. Different ♫ 
4. Different, integer division vs decimal division ♫
5. Different ♫

Rewrite the code so that it works in the REPL.

```
let z = 7
let y = z + 8
let x = y ^ 2
let waxOn = x * 5
```

1.
    1. 1135
    2. 1135
    3. -1110
    4. 1110
(Exercises are continued in the Chapter2Exercises.hs file.)

**Definitions**:

* **Parameters** are the named variables bound in the head of a function.
* **Arguments** are the values passed into functions.
* An **expression** is a syntactically valid combination of constants, functions, and variables that returns a result.
* **Values** are expressions which cannot be reduced further. `4` is a value.
* A **function** is a mapping of an input set to an output set. It accepts an argument and returns a value.
* **Infix style** is where an operator shows up between operands. 
* An **operator** is a function which is infix by default. In Haskell, operators must consist of symbols and not alphanumeric characters.
* **Syntactic sugar** is syntax in a programming language which provides the programmer a more readable, friendlier alternative to more dense syntactical constructs.

#### Chapter 3: Strings

* A string is a data structure used to represent text.
* Types are a way of _categorizing values_.
* In Haskell, **strings** are lists of characters.
* You can use the `:type` command in the REPL to view the type of something.


### 10/27/2017 (Friday)

**Pages 67-72.5 (5.5 pages)**

* `Char` is the type for single unicode characters. They are surrounded in single quotes (`'`).
* `String` is the type for Lists of Chars. They are surrounded in double quotes (`"`).
* String is a **type alias** for a List of Char.
* Lists are represented by square brackets (`[]`). These brackets are syntactic sugar.
* **Type aliases** are synonyms for types. They can be used to simplify a type signature. For example, String is a type alias for [Char].
* `main` is the default action when you build an executable. Stack requires you to have a `main` block in a `Main.hs` file, but you can load files into GHCi without them having a `main` block.
* `main` is not a function; it is a series of instructions to execute, or a _block_.
* `IO` is the type used to indicate a procedure has to do more than execute a function or evaluate an expression. It means the procedure must produce some kind of side effect, such as printing to the screen.
* `do` notation is syntactic sugar that allows you to sequence actions.
* **Concatenation** means "linking together." You can concatenate two strings, for example, by using the `++` operator or the `concat` function (which accepts a list of strings).
* **Top-level declarations** are declarations which are not nested inside anything else. They are available in the scope of the module, i.e., everywhere in the module.

### 10/28/2017 (Saturday)

**Pages 72.5-87 (14.5 pages)**

* **Local declarations** are nested inside some other expression and are only available within that expression's scope.
* To **bind** or **declare** something means to give an expression a name.

**Exercises: Scope (p.73)**

1. Yes, y is in scope for z since it is a top-level declaration
2. No; h has not been defined
3. No; r should be nested in a `where` clause. `pi` is implicitly imported from Prelude.
4. Yes; because now `r` is nested in a `where` clause.

---

Given a type signature
    
    (++) :: [a] -> [a] -> [a]

Everything after the double colon (`::`) is about our types, not our values. The variable `a` is a _type variable_. The square brackets are list _type constructors_.

The first two `[a]`s are parameters to the `(++)` operator. These are lists of some type `a`. Note that whatever the type `a` is, it must be the same type in both lists, because both contain `a` (because `a == a`).

The last `[a]` (list of `a`s) is the return type. This will be a new list which is the result of concatenating the two argument lists together.

The type `[a]` means we've got a list of elements whose type we don't know.

**The type variable `a` is _polymorphic_.**

---

* **Typeclasses** provide definitions of functions that can be shared across sets of types.

---

**Exercises: Syntax Errors (p.76)**

1. Won't compile. `++` must be wrapped in parentheses: `(++)`.
2. Won't compile. The strings need to be wrapped in double quotes, not single quotes.
3. Will compile.

---

* Top-level declarations may be exported by the module in which they reside and imported in another module.
* INDENTATION NOTE: Lines which _continue_ a declaration must be indented beyond the level of the first line.
* Local declarations are **only visible within the function to which they are associated**.
* Strings are lists; therefore, you can use standard list functions on them.

List functions:
* The _cons_ operator: `(:)` builds a list. Example: `'J' : "amie"` => `"Jamie"`
* `head` returns the first element of a list. Example: `head "Jamie"` => `'J'`. **UNSAFE**.
* `tail` returns the list without the head. Example: `tail "Jamie"` => `"amie"`. **UNSAFE**.
* `take` returns the first n characters of the list. Example: `take 5 "Mississippi"` => `"Missi"`
* `drop` returns the rest of the list after the first n characters have been "dropped." Example: `drop 5 "Mississippi"` => `"ssippi"`.
* The `(!!)` operator returns the 0-indexed element at the index specified in the list. Exmaple: `"Jamie" !! 2` => `'m'`. **UNSAFE**.

---

**Chapter 3 Exercises (pp.81-85)**

Reading syntax

1. 
    1. Correct ♫
    2. Incorrect; `++` must be wrapped in parentheses for use with prefix notation. ♫
    3. Correct ♫
    4. Incorrect. Missing `"` ♫
    5. Incorrect. List must come first. ♫
    6. Correct. ♫
    7. Incorrect. Number of elements to take must be passed as an integer argument before the string. ♫
    8. Correct. ♫
2.
    1. Matches d) ♫
    2. Matches c) ♫
    3. Matches e) ♫
    4. Matches a) ♫
    5. Matches b) ♫

Building functions

1. 
    1. `"Curry is awesome" ++ "!"` ♫
    2. `head (drop 4 "Curry is awesome!")` WRONG. This returns `'y'`, which is a character, not the string `"y"`. Granted, they probably just miswrote this question; if so, `"Curry is awesome!" !! 4` is the easiest way to get the `'y'`. But assuming they didn't miswrite it, let's try again... `take 1 (drop 4 "Curry is awesome!")`. ♫
    3. `drop 9 "Curry is awesome!"` ♫

2 - 5. (See `chapter3exercises.hs`)

6. (See Reverse.hs)

---

* **Strings** are sequences of characters. In Haskell, a String is a List of Char ([Char]).
* **Types** (or _datatypes) are classifications of values. Types specify what values inhabit them.
* **Concatenation** is the act of joining sequences of values together. In Haskell, you can use `++` and `concat` for concatenation of lists.
* **Scope** or **visibility** is the area of code in which a variable is available for use.
* **Local** bindings/declarations are declarations which can only be accessed in the specific expression in which they are declared. They _cannot_ be imported or exported on the module level.
* **Top-level** bindings/declarations are declarations which can be accessed everywhere in the module in which they are declared. They may also be exported by that module and imported by other modules.
* **Data structures** are a way of representing data so that the data may be accessed conveniently or efficiently.


### 10/29/2017 (Sunday)

**Pages 87-95.5 (8.5 pages)**

### Chapter 4: Basic datatypes

* **Types** (or _datatypes_) are a way of grouping together values that share something in common. In Haskell, every value has a type. You can think of types like mathematical _sets_.
* **Data declarations** are how datatypes are defined.
* **Type constructors** are names of types. They show up in type signatures.
* **Data constructors** (also called _value_ constructors) are the actual values that inhabit a type. They show up as the values in your code and as the values to which expressions evaluate (they show up at the _term level_).

Here is an example datatype declaration:

    data Bool = False | True

In this example, `Bool` is the name of the _type_ constructor, and `False` and `True` are the _data_ constructors. The pipe (|) reads as "or"; this says that Bools can be either False OR True. Types which may be _one_ of any of a number of things separated by pipes are known as _sum types_.

* Logical **disjunction** means OR.
* Logical **conjunction** means AND.
* In GHCi, you can view the declaration of imported datatypes by using the `:info` command.

**Exercises: Mood Swing (pp.89-90)**

    data Mood = Blah | Woot deriving Show

1. The type constructor is `Mood`.
2. You could use `Blah` or `Woot`.
3. The type signature `changeMood :: Mood -> Woot` is flawed because it uses a data constructor. Type signatures may only use type constructors. Therefore, a corrected version would be `changeMood :: Mood -> Mood`
4. (see `moodswing.hs`)
5. (see `moodswing.hs`)

---

**Pattern matching** allows you to define piecewise functions. For example:

    boolBit :: Bool -> Int
    boolBit True = 1
    boolBit _ = 0

You can match on each data constructor available in a given type. In this case, we match on `True`, then we write a **catch-all case using an underscore (_)**. The catch-all case will handle all possible values not already accounted for by earlier matches. In this case, the only possible remaining value is `False`.

---

#### Numbers

* **Integral** numbers are positive or negative whole numbers.
    * `Int` is a fixed precision (_bounded_) type for integers. It cannot hold arbitrarily large or small integers.
    * `Integer` is an unbounded type of integers. It _can_ hold arbitrarily large numbers.
* **Fractional** numbers are decimal numbers.
    * `Float` is for single-precision floating point numbers. These are dangerous to use because of rounding errors.
    * `Double` is for double-precision floating point numbers.
    * `Rational` is for numbers which can be represented as a ratio of two integers. Using this type, `1/2` will be stored as pair of integer (1 and 2), not as a decimal number. This type is arbitrarily precise.
    * `Scientific` represents numbers with scientific notation. The exponent is an Int and the coefficient is an Integer.
* **Typeclasses** are a way of adding reusable functionality to all types with an instance of that typeclass.
* You can assign a concrete type to a polymorphic value with the `::` operator, e.g., `120 :: Int8`.
* Fixed-size Int types are _bounded_ (meaning they have a minimum and maximum value) and they are represented with _two's complement_.
* You can find out the max and min bounds of a type with a Bounded typeclass instance with the `maxBound` and `minBound` values. For example: `maxBound :: Int8` and `minBound :: Int8`.

### 10/30/2017 (Monday)

**Pages 95.5-102 (6.5 pages)**

* The division function (/) is for Fractional numbers. The type is `Fractional a => a -> a -> a`. The `Fractional a =>` part signifies a _typeclass constraint_.
* The `Num` typeclass is a _superclass_ of the `Fractional` typeclass. This means that Fractional values offer all the operations of a `Num` value; the converse, however, is _not_ true.

Comparison operators:
* `(>)` - Greater than
* `(<)` - Less than
* `(==)` - Equal
* `(>=)` - Greater than or equal to
* `(<=)` - Less than or equal to
* `(/=)` - not equal

* `Ord` is the typeclass for things which can be ordered.
* `Eq` is the typeclass for things that can be compared for equality.
* In order for lists to be compared, both they and their child types must have instances of Ord or Eq (depending on if you're doing less-than, greater-than or equals comparison).
* Comparison operators only work across the same type. For example, you _can_ do `'a' == 'b'`, but you _cannot_ do `'a' == 4`
* You can define datatypes in the prelude without any special syntax! Just start with the `data` keyword as you normally would.

The datatype `Bool`, whose declaration is as follows:

    data Bool = True | False

can be read as "The datatype Bool is represented by the values True or False."

**You can ask for the type of any _data_ constructor** in the GHCi by using the `:type` command, e.g., `:type True`.

Boolean operators:
* `&&` is logical _conjunction_ (AND). Example: `True && True` is read as "true and true."
* `||` is logical _disjunction_ (OR). Example: `True || False` is read as "true or false."

### 10/31/2017 (Tuesday)

**Pages 102-106 (4 pages)**

**Exercises: Find the Mistakes (pp.101-102)**

1. `not True && True` (`t` in `true` needed to be capitalized)
1. `not (x == 6)` (use double equals for equality checking). I'm assuming you've defined `x`.
1. OK.
1. `Merry` and `Happy` are unknown data constructors. Assuming they want strings: `["Merry"] > ["Happy"]`
1. You can't concatenate differentt types. You could do `"1, 2, 3" ++ "look at me!"`

---

#### Conditionals with if-then-else

Haskell has **if expressions**. That means that they can be evaluated to a value. Example:

`if True then "True!" else "False!"`

The structure is:

```
if TEST_EXPRESSION
then EXPRESSION_A
else EXPRESSION_B
```

The `TEST_EXPRESSION` must evaluate to a `Bool`. `EXPRESSION_A` and `EXPRESSION_B` **must** evaluate to the same type.

#### Tuples

**Tuple** is a type that allows you to store multiple values in a single value. Tuples look like this: `(val1, val2, ..., valN)`. This parenthetical syntactic sugar shows up at both the type level and the term level.

The number of values inside a tuple is known as its **arity**.

The two-tuple is expressed at both type and term levels with the constructor `(,)`.

### 11/1/2017 (Wednesday)

**Pages 106-110 (4 pages)**

Look at the two-tuple datatype:

    data (,) a b = (,) a b

This data type has two important qualities:
* It accepts **type parameters**, represented by the type variables `a` and `b`. Concrete types must be passed in as arguments to the tuple type constructor as a consequence of these type parameters. Note that since the type variables are different (`a` /= `b`), you can supply different types to this constructor.
* It is a **product type**, meaning that it joins the two type variables together with logical conjunction to construct a value.

This datatype also has special syntax, as you can see with the `(,)`. You can use this syntax to pattern match tuples:

    fst' :: (a, b) -> a
    fst' (a, b) = a

#### Lists

Lists also have their own special syntax: `[]`. This shows up at the type and term level, just like tuples. Lists have variable arity, however, unlike tuples, which have a fixed, unchangeable arity. Also, elements in a list must all be the same type.

---

THURSDAY HACKATHON.

---

### 11/3/2017 (Friday)

**Pages 110-118 (8 pages)**

#### Chapter 4 Exercises

1. The type signature of length would be `length :: [a] -> Integer`. WRONG. It's actually `Foldable t => t a -> Int`. I was close-ish.
2. 
    1. `5` (Int) ♫
    1. `3` (Int) ♫
    1. `2` (Int) ♫
    1. `5` (Int) ♫
1. The first one will work and the second one will return error, because the division function `(/)` works with Fractional numbers only, and `length` returns an Int. ♫
1. You can fix it by using the `div` function, which is integer division. ♫
1. The type is `Bool`, and the value would be `True` since 5 == 5. ♫
1. `Bool`, `False` ♫
1. Answers:
    1. Will work; True ♫
    1. Won't work, because lists can't hold values of different types ♫
    1. Will work; 5 ♫
    1. Will work; True && False => False ♫
    1. Won't work; `9` is not a Bool ♫
1. Function (♫):
```
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = reverse x == x
```
9. Function (♫): 
```
myAbs :: Integer -> Integer
myAbs x = if x > 0 then x else (-x)
```
10. Function (♫):
```
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f p1 p2 = ((snd p1, snd p2), (fst p1, fst p2))
```

Correcting syntax

(see `chapter4exercises.hs`)

Match the function names to their types

1. c) ♫
1. b) ♫
1. a) ♫
1. d) ♫

---

#### Definitions

* **Tuples** are ordered groupings of values.
* A **typeclass** is a set of operations that may be performed against a polymorphic type. It is possible to define a typeclass _instance_ for your type to allow you to use your type with that typeclass's operations.
* A **data constructor** is a means of creating a value inhabiting a type. It may accept 0 arguments, in which case it is a _nullary_ data constructor, or it may accept n arguments. Data constructors may only appear at the term level.
* **Type constructors** are a way to construct _types_. They may only appear at the type level. They may also accept 0 or more arguments. They always appear at the left of the `=` sign in a data declaration.
* A **data declaration** allows you to define a new type in Haskell. They always define a new type constructor, but not always new data constructors (they may define _no_ data constructors, e.g., `data Woot`).
* **Type aliases** allow you to give a more concise and/or semantically meaningful name to an existing type. Example: `type Name = String`
* **Arity** is the number of arguments a function accepts.
* **Polymorphism** in Haskell gives you the ability to perform operations upon values of several or all types. There are two kinds:
    * **Parametric** polymorphism means a value can be _any_ type. The `id` function, for example, is parametrically polymorphic because it accepts any type: `id :: a -> a`.
    * **Constrained** or _bounded_ polymorphism means a value can be one of a specific set of types, as dictated by typeclass constraints.

---

* **Term-level** code is the code that holds values and is executed when your program is run.
* **Type-level** code is the code which specifies the types of your expressions and is statically analyzed before the program is run.
* There are **seven** named constructs in Haskell:
    * Term-level variables
    * Type variables
    * Functions
    * Typeclasses
    * Type constructors
    * Data constructors
    * Modules

### 11/4/2017 (Saturday)

### Chapter 5: Types

**Pages 118-123 (5 pages)**

* In Haskell, _there is no untyped data_.
* System F is a typed lambda calculus.
* In Haskell, **type-checking occurs at compile-time** because it is a **static** language.
* Haskell's type system allows you to write safer code which is easier to maintain and requires fewer tests than code written in a weakly-typed language.
* **The arrow (->) is the type constructor for functions in Haskell**. It's baked into the language. The data declaration has no data constructors.
* **Functions are values**.
* Function _application_ is _left associative_.
* Function parameterization (with the arrow `->` operator) is _right associative_.

### 11/5/2017 (Sunday)

**Pages 123-131 (8 pages)**

* You can check if a given type has an instance of a certain typeclass by querying the type iwth the `:info` command.
* Nothing to the left of the typeclass arrow, `=>`, shows up at the term level.
* Tuples of typeclass constraints represent **conjunctions** (ANDs).

**Exercises: Type Matching (pp.125-126)**

* `not` matches with `Bool -> Bool` ♫
* `concat` matches with `[[a]] -> [a]` ♫
* `head` matches with `[a] -> a` ♫
* `length` matches with `[a] -> Int` ♫
* `(<)` matches with `Ord a => a -> a -> Bool` ♫
* Remember, the type constructor for functions, `(->)`, is **right associative**. This means that `a -> a -> a` is the same as `a -> (a -> a)`.
* Functions in Haskell **only take one argument and return one result**. Functions which appear to accept multiple arguments are actually curried, meaning they are a series of nested functions, each of which accept one argument and return one result.
* **Currying allows you to _partially apply_ functions**.
* **Partial application** allows you to create a function with "preloaded" arguments. It's the same as binding a subset of the total number of available variables in heads in a nested lambda expression to values. Example:
```
addNumbers :: Integer -> Integer -> Integer
addNumbers x y = x + y
-- partially apply
> let add10 = addNumbers 10
> :t add10
add10 :: Integer -> Integer
> add10 5
15
```
Note that in the above example, we can still use `addNumbers`, but we now also have this convenient `add10` function.

* You can _uncurry_ functions by placing all their arguments in a tuple. For example, `(+)` would go from `Num a => a -> a -> a` to `Num a => (a, a) -> a`. 
* **Uncurried** functions are those which accept multiple arguments and return a single value.
* **Curried** functions are those which are a series of nested functions, each accepting a single argument.

### 11/6/2017 (Monday)

**Pages 131-137.5 (6.5 pages)**

* Functions that appear to accept multiple arguments are actually higher-order functions.
* A **higher-order function** returns a function value instead of a fully evaluated value (a non-function value).
* It is possible to uncurry and curry functions generically. You can write an `uncurry` function with the following type: `(a -> b -> c) -> (a, b) -> c` and you can write a `curry` function with the following type: `((a, b) -> c) -> a -> b -> c`. 
* **Sectioning** is partial application of infix operators. It lets you choose whether to apply the first or second argument of an operator. You can even section binary prefix functions by surrounding them with backticks.
* You can construct a list with a range of values between x and y by using the `..` syntactic sugar: `[x..y]`.

**Exercises: Type Arguments (pp.134-137)**

Fun fact: You can use `undefined` as a placeholder value in GHCi.

1. `Char -> Char -> Char` ♫
1. `Char` ♫
1. `Num b => b` ♫
1. `Double` ♫
1. `[Char]` ♫
1. `Eq b => b -> [Char]` ♫
1. `(Num a, Ord a) => a` ♫
1. `(Num a, Ord a) => a` ♫
1. `Integer` ♫

---

* **Polymorphism** means **"made of many forms."**

### 11/7/2017 (Tuesday)

**Pages 137.5-142 (4.5 pages)**

* "Monomorphic" means "made of one form."

There are TWO types of polymorphism:
* _parametric_ polymorphism is completely abstract, e.g., `a`. The identity function (`id`) is parametrically polymorphic. You cannot _do_ anything with the values of parametrically polymorphic type variables.
* _constrained_ or _ad-hoc_ polymorphism is a type with a typeclass instance, e.g., `Num a => a`. Typeclasses provide a set of operations, so you can _do_ more with these kinds of types.

Type signatures in Haskell take three kinds of types:
1. Concrete
2. Constrained polymorphic
3. Parametrically polymorphic

* Concrete types start with an uppercase letter.
* **Type variables represent a set of possible _types_**.
* You can _do_ the most with concrete types, and do the least with parametrically polymorphic types. Concrete types can have multiple instances of multiple typeclasses, meaning they have multiple algebras, whereas pp types have no typeclasses and therefore no algebras.
* Typeclass inheritance is possible, e.g., `Integral` inherits from the superclass `Num`.
* **Parametricity** means that for every possible concrete type a function may be given, the behavior of the function will be the same.
* Polymorphic functions are those which have one or more polymorphic type variables.

**Exercises: Parametricity (p.140)**

1. Done
2. The two possible variants are:

```
fn :: a -> a -> a
fn x y = x
```
and
```
fn :: a -> a -> a
fn x y = y
```
3. There is one possible implementation:

```
abb :: a -> b -> b
abb a b = b
```

The behavior does not change when the types of `a` or `b` change.

---

* **Polymorphic constants** are values which may resolve to several different types. For example, `5` is a polymorphic constant; it could resolve to be an `Int`, a `Fractional`, an `Integral`, and more, all depending on the context in which it is used. By default its type is `Num a => a`.
* You can force an expression to be of a certain type by annotating it with `::`, e.g., `5 :: Int`.
* In general, the compiler will always infer the most abstract possible type.
* You can use the `fromIntegral` function (and its sister functions) to force a specific numeric type to be more abstract. For example, `fromIntegral` forces an `Integral` number to implement the `Num` typeclass so that you can use it with other types of numbers than just integral numbers.

### 11/8/2017 (Wednesday)

**Pages 142-151 (9 pages)**

* Haskell has _type inference_.
* **Type inference** gives a language the ability to guess the type of a given expression.
* Haskell's type inference algorithm will infer the most general (polymorphic) type, starting with the types it already knows and working from there.

**Exercises: Apply Yourself (p.145)**

1. `myConcat :: [Char] -> [Char]` ♫
2. `myMult :: Fractional a => a -> a` ♫
3. `myTake :: Int -> [Char]` ♫
4. `myCom :: Int -> Bool` ♫
5. `myAlph :: Char -> Bool` ♫

---

#### Asserting types

You can assert types with the `::` syntax.

REPL example:

```> let triple x = x * 3 :: Integer```

Source file example (most common):

```
-- type declaration
triple :: Integer -> Integer
-- function declaration
triple x = x * 3
```

It is also possible to declare the type of a declaration in where and let clauses:

where:
```
triple x = triple' x
  where triple' :: Integer -> Integer
        triple' x = x * 3
```

let:
```
triple x = let triple' :: Integer -> Integer
               triple' x = x * 3
           in
             triple' x
```

Of course, you cannot force arbitrary types into other arbitrary types. For example, this won't work:

WRONG
```
let x = 5 :: String
```
WRONG

because Strings don't have an instance of the Num typeclass.

**Chapter 5 Exercises (!) (pp.148-157)**

**Multiple choice**

1. c), a list whose elements are all of some type `a`
2. a), take a list of strings as an argument
3. b), returns one element of type `a` from a list
4. c), takes a tuple argument and returns the first value

Aside: **Top-level declarations without type signatures will be forced into concrete types if possible due to the _monomorphism restriction_.** You can turn this restriction off by placing this at the beginning of your file:

```
{-# LANGUAGE NoMonomorphismRestriction #-}
```

**Determine the type**

These are saved in Chapter5Exercises.hs.

1. 
    * Value: `54` (I'm a dumbass). Type: `Num a => a`. ♫
    * Value: `(0,"doge")`. Type: `Num a => (a, [Char])` ♫
    * Value: `(0,"doge")`. Type: `(Integer, [Char])` ♫
    * Value: `False`. Type: `Bool` ♫
    * Value: `5`. Type: `Int` ♫
    * Value: `False`. Type: `Bool` 
2. `Num a => a` ♫
3. `Num a => a -> a` ♫
4. `Fractional a => a` ♫
5. `[Char]` ♫

**Does it compile?**

1. This won't compile because `bigNum` is not a function and the first argument of `$` must be a function. We can fix it as follows (♫):
    ```
    bigNum = (^) 5
    wahoo = bigNum 10
    ```

2. No errors. ♫
3. This won't compile because `b` is not a function, and neither is `c`. We could rewrite it as follows (♫):
    ```
    a = (+)
    b = 5
    c = a 10
    d = c 200
    ```
4. The first expression won't compile in the REPL because `b` is not defined yet; the second expression won't compile because `c` is not defined. We could rewrite it as:
    ```
    c = 20
    b = 10000 * c
    a = 12 + b
    ```

### 11/9/2017 (Thursday)

**Pages 151-157.5 (6.5 pages)**

(continuing the chapter 5 exercises)

**Type variable or specific type constructor?**

2. `zed`: fully polymorphic; `Zed`: concrete; `Blah`: concrete
3. `a`: fully polymorhpic; `b`: constrained (Enum); `C`: concrete
4. `f`, `g`: fully polymorphic; `C`: concrete

**Write a type signature**

1. `[a] -> a` ♫
2. `(Ord a, Ord b) => a -> b -> Bool` WRONG. They must both be the same type: `Ord a => a -> a -> Bool`. This is  because the `(>)` function requires that both operands be of the same type.
3. `(a, b) -> b` ♫

**Given a type, write the function**

1. `i a = a` ♫ (this is the identity function)
2. `c a _ = a` ♫
3. Yes, `c''` and `c` are identical.
4. `c' _ b = b` ♫
5. could be `tail`, `reverse`, `init`... ♫
6. `co bToC aToB = bToC . aToB` ♫ (bonus! point free form!)
7. `a _ x = x` ♫
8. `a' aToB = aToB` ♫ (bonus! point free form!)

**Fix it**

1. (see sing.hs)
2. (see sing.hs)
3. (see arith3broken.hs)

**Type-Kwon-Do**

1. `h = g . f` ♫
2. `e = w . q` ♫
3. `xform (x, y) = (xz x, yz y)` ♫
4. `munge xToY yToWZ x = fst $ yToWZ $ xToY x` ♫

### 11/10/2017 (Friday)

**Pages 157.5-163 (5.5 pages)**

**Definitions**

* _Polymorphism_ means "made of many forms." It refers to type variables which may be more than one concrete type. In Haskell, there are two types of polymorphism:
    * _ad-hoc_ or _constrained_ polymorphism, which refers to type variables that have one or more typeclass constraints. **Haskell requires that (type, typeclass) pairs be unique** (in scope), so you cannot have two instances of the `Num` typeclass for the `Int` type, for example. This is very good; it makes reasoning about constrained variables much easier.
    * _parametric_ polymorphism, which is a fully polymorphic type variable with no constraints
* _Type inference_ is an algorithm for guessing the type of a given expression based on things already known about that expression (e.g., what concrete types, if any it uses). In Haskell, type inference will guess the principal type.
* The _principal type_ is the most generic type an expression can have and still typecheck.
* A _type variable_ is a variable occuring in a type signature which represents some type or set of types. For example, in `id :: a -> a`, `a` is the type variable.
* A _typeclass_ provides an algebra (set of operations) that may be associated with various types through instantiation. Typeclasses allow us to operate on any type with that typeclass instance without having to write special code for it. In other words, you can write a function `add :: Num a => a -> a -> a; add x y = x + y` which will work with _every type that instantiates the Num typeclass_, as opposed to having to write the `add` function for every possible type it could accept (such as Int, Integer, Float, Double, Rational, etc). 
* _Parametricity_ describes functions whose behavior is the same no matter what argument types are passed to them because their parameter types are parametrically polymorphic.
* _Modules_ are a way of organizing code. They can contain imports, data declarations, constants, function definitions -- basically anything. Example (in a source file): `module Bar where`
    * You can import a function named `foo` from a module named `Bar` with `import Bar (foo)`. Note that this will also import all typeclass instances defined in the module `Bar`
    * You can import _everything_ exported from a module named `Bar` with `import Bar`
    * Note that **you may only define one module per file**.

### 11/11/2017 (Saturday)

**Pages 163-169.5 (6.5 pages)**

* A type declaration defines a particular type and what it looks like; a typeclass definition defines how a particular set of types (those with instances of the typeclass) may be _consumed_ in code.
* A typeclass instance for a given type defines how the functions from that typeclass work for that type. 
* The `Eq` typeclass is for types that can be compared for equality
* The `Ord` typeclass is for types that can be ordered
* The `Bounded` typeclass is for types that have a `maxBound` and `minBound`
* The `Show` typeclass is for types that can be represented as strings
* The `Enum` typeclass is for types that can be enumerated
* Typeclasses have a hierarchy. For example, for something to have an instance of `Ord`, it must first have an instance of `Eq`.
* **Don't use the `Read` typeclass.**
* Once you apply an argument to a typeclass function, its type is specialized according to the type of that argument. For example, `(==) "cat` becomes `[Char] -> Bool`.
* The type of a type variable is usually set by its leftmost occurence. That means that if you have a function with the type `a -> a -> a` and provide an `Int` as its first argument, the type of `a` is set to `Int` _throughout the entire expression_.
* You can _derive_ the following typeclasses: `Eq`, `Ord`, `Enum`, `Bounded`, `Read`, and `Show`. 
* **Deriving** a typeclass for a type means that the type inherits the methods from that typeclass without you having to write definitions for them. This only works for some typeclasses (listed above) and there are other constraints on it.

### 11/12/2017 (Sunday)

**Pages 169.5-180.5 (11 pages!)**

**Writing typeclass instances**

* You can view documentation for things such as typeclasses on hackage.haskell.org.
* Typeclass documentation will tell you the "minimum complete definition" of that typeclass. **The minimum complete definition** is the functions you must implement for your instance to be valid. For `Eq`, this is _either_ `(==)` or `(/=)`.
* Haskell doesn't provide universal stringification or equality checking, because this is not always safe. Haskell catches errors like trying to check the equality of two datatypes that do not implement `Eq` at _compile time_.

See `trivial.hs` for an example data type and `Eq` typeclass instance.

* **Partial functions** are functions which do not handle all possible inputs. For example, the following is a partial function: `add 1 2 = 3`.
* By default, GHC does not complain about partial functions. However, you can make it spit out warnings about them by **turning on all warnings with the `-Wall` flag**: `>:set -Wall`
* If you want to handle a small set of cases, create a sum type! Don't use `Int`s as an enum like C programmers do.
* **NOTE: You use the datatype's _type constructor_ in the _typeclass instance declaration_ and the _data constructor(s)_ in the typeclass instance's _function definitions_.**
* Also note: When you write an instance for a type that accepts type variables, **you must use the fully applied type**. For example, if you have a type `data Pair a = Pair a a`, then you must use `(Pair a)` in the instance declaration.

Sometimes you will want to write a typeclass instance that requires that its datatype's polymorphic variables also have instances of some typeclass. To do this, you must add a typeclass constraint to that variable in the instance declaration. Example:

```
-- datatype declaration
data Identity a = Identity a

-- notice the typeclass constraint: Eq a => ...
instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'
```

**Exercises: Eq Instances (pp.178-179)**

(see eqInstanceExercises_chapter6.hs)

♫ ♫ ♫

---

* **Typeclass inheritance is strictly additive**. This means that typeclass instances which inherit from superclasses _may not_ override methods from those superclasses.

### 11/13/2017 (Monday)

**Pages 180.5-186.5 (6 pages)**

**Exercises: Tuple Experiment (p.180)**

I think `quotRem` returns a tuple with the resultant quotient of integer division that rounds towards zero and the remainder of that division. I think `divMod` does the same thing, except that the division rounds down, and `mod` uses the sign of the divisor. RIGHT. `rem` uses the sign of the dividend. Ding ding!

* When constraining a type by a typeclass which inherits from another typeclass, you need only provide the child typeclass. For example: `divideAndAdd :: Fractional a => a -> a -> a` is perfectly valid; you don't need to do `(Num a, Fractional a) =>` because that would be redundant. The types which implement the `Fractional` typeclass are a subset of the types which implement the `Num` typeclass.
* Polymorphic values must be forced to a concrete type when applied.
* There are default concrete types for constrained polymorphic values which are used when GHC must force a constrained value into a concrete type without sufficient information to nail down exactly _which_ type. These defaults are specified in the libraries which implement the typeclasses.
* The Haskell Report specifies default types for numeric typeclasses.
* GHC will complain when there is no default concrete type and it cannot infer one based on context.
* We can narrow a polymorphic function down to a monomorphic function by applying concrete types. However, we _cannot_ broaden a monomorphic function back up to a polymorphic function.

### 11/14/2017 (Tuesday)

**Pages 186.5-194.5 (8 pages)**

#### Ord typeclass

* The `Ord` typeclass is for things that can be ordered.
* `Ord` inherits from the `Eq` typeclass. **Eq is a superclass of `Ord`**.
* When you derive `Ord` on a sum type, the "value" of items increases from left to right, like a number line. So for example, if you have: `data CrazyWords = ding | wololo | flerm`, then `compare ding flerm == LT`, meaning that `flerm` is greater than `ding` because it is to the _right_ of `ding`.
* **`Ord` instances should agree with the corresponding `Eq` instances**.
* Do not write partial instances of `Ord`.
* You should add the _minimally sufficient_ set of constraints to type variables in your function definitions. However, it is possible to "over-constrain" by constraining a variable with a class that inherits from the minimally sufficient superclass.

**Exercises: Will They Work? (p.192)**

1. Yes, this will work. ♫
2. Yes, this will work. ♫
3. This will not work. The types being compared are different. ♫
4. Yes, this will work. ♫

#### Enum

* Enum is a typeclass which defines operations for enumerable types whose values have well-defined successors and predecessors.

I think `enumFromThenTo` takes a starting value, a "then" value, and a "to" value and constructs a list which contains items that jump from the starting value by the number of spaces between `then - start`. ♫

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
* **Lexical scoping** means that variable value resolution is based on the location in the code that variable appears and what the _lexical context_ is. The advantage of lexical scoping is that variable scope can be understood merely by reading program text, rather than having to run it.

### 11/20/2017 (Monday)

**Pages 221.5-229 (7.5 pages)**

* The lexically _innermost_ binding for a variable of a particular name always takes precedence. (quote)
* GHCi actually uses a series of nested lambdas to hold variable definitions.

#### Anonymous functions

* **Anonymous functions** are functions without names. 
* You can construct an anonymous function by using the lambda syntax (`\`). For example, given this _named_ function:
    ```
    double :: Integer -> Integer
    double x = x * 2
    ```
    The equivalent _anonymous_ function is
    ```
    (\x -> x * 2) :: Integer -> Integer
    ```
* In GHCi error messages, `it` refers to the last expression you entered.
* You will usually have to wrap lambdas in parentheses to make it clear how they're being used: `(\x -> x * 2) 2` will work but `\x -> x * 2 2` will not because it thinks you're trying to apply 2 to 2.
* You can use `let` in GHCi multi-line mode to define things by indenting the remaining lines of the declaration to the same level as the beginning of the first line after `let`. E.g.,
    ```
    let f :: Int -> Int
        f x = x * 2
    ```

**Exercises: Grab Bag (pp.224-225)**

1. All of the expressions are equivalent. ♫ (compared the types)
2. `Num a => a -> a -> a` ♫
3. 

a) 
```
addOneIfOdd' n = case odd n of
  True -> f n
  False -> n
  where f = \n -> n + 1
```
♫

b) `addFive = \x -> \y -> ((if x > y then y else x) + 5)` ♫

c) `mflip f x y = f y x` ♫

---

#### The utility of lambda syntax

* In Haskell, **named entities evaluate differently than anonymous entities**. This is one reason why you might choose lambda syntax over named functions.
* You may also choose to use an anonymous function when you are passing it as an argument to another function and you are not going to use it anywhere else in your code.

#### Pattern matching

* **Pattern matching** is a means of matching values against patterns and optionally binding variables to successful matches.
* Pattern matching works on _any and all data constructors_.
* Patterns are matched against **values**, **not types**.

Example function matching on a number:

```
isItTwo :: Integer -> Bool
isItTwo 2 = True
isItTwo _ = False
```

Notice the lines below the type signature. This is pattern matching. We can match on the number `2`, or we can match on _any other number_ using the `underscore`, which matches any value. It would match 2, as well, if it didn't come _after_ the 2-matching pattern. Underscores are often used as "otherwise" or "anything-else" cases.

Moreover, **the ordering of patterns matters**. If you define the catch-all case first in a series of patterns, it will _always match_ and the other patterns will be ignored.

* **Order patterns from _most_ specific to _least_ specific.**
* Incomplete pattern matches will throw an exception if they are applied to unmatched data! Therefore, **it is _very_ important that you match all possible values**.
* Incomplete pattern matches will also return _bottom_ when applied to unmatched data. _Bottom_ is a "non-value" which denotes that the program cannot return a result.
* You can set the `-Wall` option in GHCi with `:set -Wall` to turn on warnings about incomplete patterns at compile-time. This option actually turns on _all_ warnings. Don't ignore them!

### 11/21/2017 (Tuesday)

**Pages 229-233.5 (4.5 pages)**

#### Pattern matching against data constructors

* You can use pattern matching to match on data constructors and their contents, even if said constructors are sum types.
* `newtype` declarations allow only one data constructor with one parameter. (?)
* **Data constructors may wrap other types, each with their own data constructors**. 
* You can match on multiple data constructors for the same type in the case of a sum type.
* **Data constructors are functions** which may accept other data constructors as arguments.

See penguins.hs, registeredUser1.hs, and registeredUser2.hs for some cool examples of pattern matching. 

Here's an example of pattern matching on a data constructor:

```
data Sport = PingPong | Hockey | Soccer

data Player = Player Sport

isSoccerPlayer :: Player -> Bool
isSoccerPlayer (Player Soccer) = True
isSoccerPlayer _             = False
```

Note that you must wrap the data constructor in parentheses
when it has a parameter. Otherwise, GHC thinks you're trying to
define a function that accepts multiple arguments.

### 11/22/2017 (Wednesday)

**Pages 233.5-238.5 (5 pages)**

* You can pattern match on tuples.
* You can view imports from a specific module in ghci by using `:browse <ModuleName>`

**Exercises: Variety Pack (pp.235-236)**

1. 

a) `k :: (a, b) -> a` ♫

b) `[Char]`; no, it is a different type than both k1 and k3. ♫

c) `k1` and `k2` both return the number `3`. ♫

2. 
```
f :: (a, b, c) 
  -> (d, e, f)
  -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))
```
♫

---

#### Case expressions

* Case expressions are another way of writing functions so that they return different results for different inputs. You can use them with any data type that has "visible" data constructors.
* Remember, **write functions that handle all possible inputs!** If you have a sum type with two data constructors, write your function to account for both constructors!

Here is an example case expression:

```
beepIfTrue :: Bool -> String
beepIfTrue b =
  case b of
    True -> "Beep!"
    False -> "Boop."
```

### 11/23/2017 - THANKSGIVING! Day off.

### 11/24/2017 (Friday)

**Pages 238.5-249 (10.5 pages)**

**Exercises: Case Practice (pp.238-239)**

(see `caseExercises.hs`)

All correct! Woot!

---

#### Higher-order functions

* **Higher-order functions** (HOFs) are functions that accept functions as arguments.
* When writing a type signature for an HOF, you must wrap the function parameter(s) in parentheses. Example: In `hof :: (a -> b) -> a -> b`, `(a -> b)` is a function parameter.
* Remember, **`->` associates to the right**. That means that `a -> b -> c` is equivalent to `a -> (b -> c)`.
* In Haskell, you can write multi-line strings by using backslashes like so:
```
"This is a\
\ multi-line\
\ string!"
```
* Higher-order functions are useful for:
    1. Manipulating the way in which functions are applied with respect to their arguments
    2. Making functions more configurable by accepting function parameters
       for key pieces of logic rather than hardcoding that logic.

**Exercises: Artful Dodgy (pp.246-247)**

1. (example)
2. `dodgy 1 1 == 11` ♫
3. `dodgy 2 2 == 22` ♫
4. `dodgy 1 2 == 21` ♫
5. `dodgy 2 1 == 12` ♫
6. `oneIsOne 1 == 11` ♫
7. `oneIsOne 2 == 21` ♫
8. `oneIsTwo 1 == 21` ♫
9. `oneIsTwo 2 == 22` ♫
10. `oneIsOne 3 == 31` ♫
11. `oneIsTwo 3 == 23` ♫

Nice!

---

#### Guards

**If-then-else review**

You can write an if-then-else expression like this:

`if <expression that evalutes to a Bool> then <result if True> else <result if False>`

This doesn't necessary need to be all on the same line. Here's an example of a multi-line if expression:

```
if hockeyPlayer
then "Cool sport, man."
else "I don't even"
```

Note that you can optionally indent the `then` and `else` lines beyond the `if` line.

### 11/25/2017 (Saturday)

**Pages 249-254 (5 pages)**

* **Guard syntax is another way for us to write piecewise functions**. It is more powerful than pattern matching because you branch on arbitrary conditions rather than values.

Example function using a _guard block_:
```
myAbs :: Integer -> Integer
myAbs x
  | x < 0     = (-x)
  | otherwise = x
```

In the example above, there are two _guards_:
1. `x < 0 = (-x)`
2. `otherwise = x`

The conditions for both guards evaluate to a Bool; `otherwise` is another name for `True`.
It's a "catch-all" case, just like the underscore (_) in pattern matching.

* **Guards are always evaluated sequentially** so you should always order them from most restrictive to least restrictive.
* Guards must always evaluate to a Bool, but their return values can be anything (so long as they match the type specified by the function's type signature).
* You can use `where` declarations in conjunction with guard blocks. See the following example:
```
type CountryCode = String

favoriteFood :: CountryCode -> String
favoriteFood c
 | c == "us" = usFavorite
 | c == "jp" = japanFavorite
 | c == "fr" = franceFavorite
 | otherwise = unknown
 where
   usFavorite = "Hamburgers!"
   japanFavorite = "Sushi ^^"
   franceFavorite = "Caviar"
   unknown = "Who knows?"
```

Note that you can use the variables defined in the where declaration
anywhere in the preceeding guard block -- both in the conditionals and the return expressions.

* GHCi can't always detect partial functions, so it's a good idea to always use an `otherwise` guard when defining your guard block.

**Exercises: Guard Duty (pp.252-253)**

(see chapter7_guardDutyExercises.hs)

1. When you make the `otherwise` guard the top-most one in the `avgGrade`, the function always returns `'F'` no matter what number you pass to it.
2. When you reorder the guards, the function still compiles and works the same. 90 stil returns 'A'.
3. The `pal` function returns `True` when `xs` is a palindrome. ♫
4. `pal` takes an argument of type `[a]`. PARTIALLY WRONG. It's actually `Eq a => [a]` because you need to be able to check the equality of the reversed list against the original list.
5. `Eq a => [a] -> Bool` ♫
6. c), an indication of whether its argument is a positive or negative number or zero. ♫
7. `numbers` takes one argument of type `Num a => a`. WRONG. It's actually `(Ord a, Num a)`. The `Num` typeclass doesn't inherit from either `Eq` nor `Ord`, so you need the `Ord` instance to get all the comparison operations the function requires (remember, `Ord` inherits from `Eq` so it would be redundant to use `(Num a, Ord a, Eq a)`).
8. `Num a => a -> a` WRONG. It's actually `(Num a, Ord a, Num b) => a -> b`

---

#### Function composition

* Function composition is a HOF which allows us to combine functions together by passing the return value of one function to another (after the first function has been applied to arguments).

### 11/26/2017 (Sunday)

**Pages 254-260 (6 pages)**

The function composition operator is the period (.). Here's how you use it:

```(f . g) x```
This is equivalent to `f (g x)`. A good way to remember how the function composition operator works is to read it as "circles". In our example:


```
(f . g) x
-- we can say
-- f "circles" g
-- which is a good way of remembering
-- the parenthesization, which kind of looks like
-- circles: f (g x)
```

* You can think of function composition as being a way of pipelining data through multiple functions. (quote)
* Remember, **function application has the highest precedence**: 10 (out of 10)! That means sometimes you need to use the `$` operator or parentheses to delay function application.
* The composition operator allows us to easily nest more than two functions together -- an operation which can be tricky when using parentheses.
* In general, when reading composed functions, you will want to read from **right-to-left**. For example: `take 5 . filter odd . enumFrom $ 3` can be read as "enumerate the integers above 3 into a list, filter out the odd ones, and take only the first 5 from that list". (Remember that Haskell is a non-strict language; this kind of expression wouldn't be possible with a strict language, because it would create an infinite list. ?)

#### Pointfree style

* Another advantage of function composition is that it allows us to write our functions in _pointfree style_.
* **Pointfree style** is a way of composing functions without referencing their arguments. The "point" refers to the arguments, therefore, pointfree ~= argument free.
* Note: `f . g` is equivalent to `\x -> f (g x)`
* You can write pointfree functions _without_ function composition. There are some cases, however, where function composition is necessary to make pointfree style possible.
* Composed functions are applied from right to left.

#### Demonstrating composition

* The functions `print` and `putStrLn` behave differently, despite the fact that they seem superficially similar.

### 11/27/2017 (Monday)

**Pages 260-263.5 (3.5 pages)**

The `print` function is a combination of converting a value into a string (`show`) and printing that string as a line to the console (`putStrLn`). Therefore, you can implement `print` as a composition of the two:

```
print :: Show a => a -> IO ()
print = putStrLn . show
```

**Chapter 7 exercises (pp.262-266)**

**Multiple choice**

1. d) 
2. b) ♫
3. d) ♫
4. b) ♫
5. a) ♫
 
### 11/28/2017 (Tuesday)

**Pages 263.5-275 (11.5)**

**Chapter 7 exercises (cont.)**

**Let's write code**

1. 
    * (see chapter7exercises.hs)
    * Yes, the type is the same
    * (see chapter7exercises.hs)
2. (see chapter7exercises.hs)
3. (see chapter7exercises.hs)
4. (see arith4.hs)
5. (see arith4.hs)
6. (see arith4.hs)

---

#### Chapter Definitions

* **Bindings** are links between names and values. In the context of function parameters, you _bind_ an argument _value_ to a parameter _name_ (in the scope of the function). In a larger sense, any collection of "bindings" is just an assortment of values (which may be functions) that can be referenced by name.
* An **anonymous function** is an unbound function, i.e., a function without a name. Example: `\x -> x`. Anonymous functions are useful when you need a one-off function (you only need to use it in one place).
* **Currying** is the process of transforming a function which accepts multiple arguments to a series of nested functions which each take one argument. In Haskell, all functions are curried by default.
* **Pattern matching** is a way of deconstructing sum types and product types to access their contents. It allows you to grab the values out of product types, and to handle the various possible values of sum types.
* **Bottom** is a "non-value" which indicates that a program cannot produce a result. This may occur, for example, when you pass an unmatched value to a partial function, or when a program loops infinitely.
* **Higher-order functions** (HOFs) are functions which accept a function as an argument and/or return a function as their result.
* **Composition** is application of the result of one function to another function. For example: `f (g x)`. The composition operator allows us to compose functions without referencing their arguments: `f . g`
* **Pointfree** style is a way of writing functions without directly referencing their arguments. This goes hand in hand with composition: `f = g . h` is pointfree, but `f x = g (h x)` is not. The "point" in pointfree means "argument."

### 11/29/2017 (Wednesday)

### Chapter 8: Recursion

Woo!

**Pages 275-282.5 (7.5 pages)**

#### Recursion

* **Recursion** is defining a function which calls itself.
* Recursion gives us a way to express _indefinite_, incremental computations.
* The **Y combinator** (or _fixed-point combinator_) allows us to write recursive functions in the lambda calculus. Haskell's recursive abilities are based on this combinator.

#### Factorial!

* The **factorial** function is a mathematical function for multiplying a number by incrementally lesser numbers until we reach 0. It is expressed by `!`. For example: `4! = 4 * 3 * 2 * 1 = 24`.
* The factorial function is a good starting point for thinking recursively.
* A **base case** in a recursive function is a case for a set of inputs at which the function should stop recursing and return a result. 
* Recursion is similar to composition in that you pass the result of one function call to another, but in recursion, you're passing the result to the same function. Recursion is indefinite, whereas composition is definite.
* Recursion is _self-referential composition_.
* Since Haskell is built on the lambda calculus, the only way we can "evaluate" something is by **applying** a function to an argument. This is all we _can_ do.

Remember: You apply a _function_ to an _argument_, not the other way around.

* A recursive function only terminates based on its inputs. If no base case is defined, then it will never terminate.

### 11/30/2017 (Thursday)

Mark's last day + drinks!

### 12/01/2017 (Friday)

**Pages 282.5-290.5 (8 pages)**

**Intermission: Exercise (pp.282-283)**

Remember, applyTimes is defined as follows:

```
applyTimes 0 f b = b
applyTimes n f b = f (applyTimes (n-1) f b)
```
```
applyTimes 5 (+1) 5 =
applyTimes (n=5) (f=(+1)) (b=5)
```
=>
```
(+1) (applyTimes (5 - 1) (+1) 5) =
(+1) ((+1) (applyTimes (4 - 1) (+1) 5)) =
(+1) ((+1) ((+1) (applyTimes (3 - 1) (+1) 5))) =
(+1) ((+1) ((+1) ((+1) (applyTimes (2 - 1) (+1) 5)))) =
(+1) ((+1) ((+1) ((+1) ((+1) (applyTimes (1 - 1) (+1) 5))))) = 
(+1) ((+1) ((+1) ((+1) ((+1) 5)))) =
(+1) ((+1) ((+1) ((+1) 6))) =
(+1) ((+1) ((+1) 7)) = 
(+1) ((+1) 8) = 
(+1) 9 =
10
```
♫

#### Bottom

The symbol `⊥` represents _bottom_.
**Bottom** refers to computations that don't/can't result in a value.

There are two main causes for bottom:

1. Computations which cause an **error** (this may be thrown by the function, or it may occur for some other reason, like applying a partial function to an unmatched argument).
2. Computations which **never terminate** (this can easily happen with recursive functions if you don't define a base case).

* You can use the `Maybe` datatype to define a function which returns a value which indicates that the computation has nothing interesting to return. 
* `Maybe` looks like this: `Maybe a = Nothing | Just a`

Here's an example function using Maybe:

```
f :: Bool -> Maybe Int
f False = Just 0
f _     = Nothing
```

Notice how in the case where f is not False, we return `Nothing`. The use of Maybe makes most uses of bottom unnecessary.

#### Fibonacci numbers

* The **fibonacci sequence** is another prime candidate for demonstrating recursion. It is a recursive mathematical sequence where every value is the sum of the previous two values before it. It is an _indefinite_ computation.

Steps for defining a recursive function (in Haskell):
1. Consider the types - What type should the function have?
2. Consider the base case - For what set of inputs should the function terminate?
3. Consider the arguments - What arguments should the function accept? How should they be used?
4. Consider the recursion - How should the function call itself to perform the computation?

#### Integral division from scratch

* You can think of integral division like repeated subtraction.
* In `10 / 5 = 2`, `10` is the _numerator_, `5` is the _denominator_, and `2` is the _quotient_.
* **The `type` keyword defines a type _synonym_ or type _alias_ for a given type**. Example: `type Numerator = Integer`

### 12/02/2017 (Saturday)

**Pages 290.5-295.5 (5 pages)**

Let's talk about division.

20 divided by 4 is

20 - 4 (16)
   - 4 (12)
   - 4 (8)
   - 4 (4)
   - 4 (0)
0 is less than 4, so we stopped. 
The result is the number of times we subtracted, which is 5.

* In Haskell, a common idiom is to use a function called `go` defined in a `where` clause for recursive functions which actually require an extra argument that they do not accept. See `division.hs` for an example of this.

**Chapter 8 Exercises (pp.294-298)**

**Review of types**

1. d) `[[Bool]]` ♫
2. b) ♫
3. d) ♫
4. b) ♫

**Reviewing currying**

### 12/3/2017 (Sunday)

**Pages 295.5-301 (5.5 pages)**

```
flippy :: String -> String -> String
flippy = flip cattyConny
```
♫

```
appedCatty :: String -> String
appedCatty = cattyConny "woops"
```
♫

```
frappe :: String -> String
frappe = flippy "haha"
```
♫

1. `"woops mrow woohoo!"` ♫
2. `"1 mrow haha"` ♫
3. `"woops mrow 2 mrow haha"` ♫
4. `"woops mrow blue mrow haha"` ♫
5. ("pink mrow haha") ++ " mrow " ++ ("green" ++ " mrow " ++ "woops mrow blue") = `"pink mrow haha mrow green mrow woops mrow blue"` ♫
6. ("are mrow Pugs") ++ " mrow " ++ "awesome" = `"are mrow Pugs mrow awesome"`

**Recursion**

Number 1: 

```
15 - 2; count = 1, n = 13
   - 2; count = 2, n = 11
   - 2; count = 3, n = 9
   - 2; count = 4, n = 7
   - 2; count = 5, n = 5
   - 2; count = 6, n = 3
   - 2; count = 7, n = 1
-- n (1) < d (2) so now we return (count, n)
(7, 1)
```

♫

Number 2:

```
sumTo :: (Eq a, Num a) => a -> a
sumTo 0 = 0
sumTo n = n + (sumTo (n - 1))
```

♫

Number 3:

```
mult :: (Integral a) => a -> a -> a
mult x 1 = x
mult x y = x + (mult x (y - 1))
```

♫

**Fixing dividedBy**

See the `dividedBy'` function in `chapter8exercises.hs`.

**McCarthy 91 function**

See the `mc91` function in `chapter8exercises.hs`

**Numbers into words**

See `chapter8exercises_numbersIntoWords.hs`

Nice work!

--- 

#### Definitions

* **Recursion** is a way of computing results using an indefinite number of steps and repeated self-referential function application. Recursive functions call themselves at least once and usually have at least one _base case_.
* A **base case** is a case for a recursive function that matches a certain set of inputs and causes the function to terminate.

### Chapter 9: Lists

WOOOOOOO!

### 12/4/2017 (Monday)

**Pages 301-305.5 (4.5 pages)**

Lists have two purposes:
1. To hold a finite sequence of values
2. To act as an infinite series of values, thereby taking the place of a _stream_ datatype.

The **List** datatype is defined as follows:

```
data [] a = [] | a : [a]
```

This has the following implications:
* Brackets -- `[]` -- are the _type constructor_ for lists.
* _Empty_ brackets -- `[]` are the _data constructor_ for lists.
* The List datatype has two data constructors, `[]` (empty list) and `:` (called "cons"[truct]).
* The cons data constructor is used for _constructing_ lists, like so: `1 : []` => `[1]`.
* The `a` and `[a]` in the definition for the cons data constructor are _type_ variables.
* The List datatype is actually recursive! The `[a]` actually references the type being defined! It is in this way that you can construct arguments of arbitrary length.

#### Pattern matching on lists

You can pattern match on list data constructors just like any other data constructor. Example:

`myHead (x : _) = x`

The above example matches the first item of a list and ignores the rest.

`myTail (_ : xs) = xs`

This example matches the last _n_ items of a list and ignores the first.

Both of these functions throw an error when passed an empty list. This can be solved by adding a base case that matches the empty list, OR, better, by _using Maybe_. Remember that Maybe is defined like so:

`data Maybe a = Nothing | Just a`

We can rewrite our functions as follows:

```
myHead :: [a] -> Maybe a
myHead [] = Nothing
myHead (x:_) = Just x

myTail :: [a] -> Maybe [a]
myTail [] = Nothing
myTail (x:[]) = Nothing
myTail (_:xs) = Just xs
```

#### List's syntactic sugar

Lists have syntactic sugar which allows you to write

`[1,2,3,4,5]`

instead of 

`(1 : 2 : 3 : 4 : 5 : [])`

* A **cons cell** is a "conceptual space that values may inhabit." ???
* A list **spine** is the "connective structure that holds the cons cells together and in place." ???

**Pages 305.5-309 (3.5 pages)**

#### Using ranges to construct lists

* _Range_ syntax is syntactic sugar for constructing lists from a range of values. For example: `[1..5]` or `['a'..'z']` or `[1,3..9]`.
* Ranges are desugared into functions from the `Enum` typeclass, such as `enumFromTo` and `enumFromThenTo`.

Desugaring:

* `[1..10]` becomes `enumFromTo 1 10`
* `[1,3..9]` becomes `enumFromThenTo 1 3 9`

Note that there are two functions from this typeclass which can produce indefinitely long lists:
* `enumFrom`
* `enumFromThen`

**Exercise: EnumFromTo (pp.307-308)**

See `chapter9_enumFromTo.hs`.

---

#### Extracting portions of lists

Three useful functions for, well, extracting portions of lists:

* `take :: Int -> [a] -> [a]` - Returns the first _n_ elements of a list as a new list.
* `drop :: Int -> [a] -> [a]` - Returns the elements after the first _n_ elements as a new list.
* `splitAt :: Int -> [a] -> ([a], [a])` - Returns a pair with the list broken in two based on the 1-index passed.

### 12/6/2017 (Wednesday)

**Pages 309-313.5 (4.5 pages)**

* You can use `take` with infinite lists.

We have two other useful functions:
* `takeWhile :: (a -> Bool) -> [a] -> [a]` - Takes elements until the function returns false. If the function returns false for the first element, returns `[]`.
* `dropWhile :: (a -> Bool) -> [a] -> [a]` - Drops elements until the function returns false. If the function returns false for the first element, returns the whole list.

**Exercises: Thy Fearful Symmetry (pp.311-312)**

See `chapter9_thyFearfulSymmetry.hs` and `poemLines.hs`.

#### List comprehensions

* **List comprehensions** allow you to create new lists from another list or lists. They come from set comprehensions in mathematics (the syntax is even similar).
* List comprehensions require at least one input list, called the **generator**. The elements in this list may be transformed or filtered out by various functions before making it into the generated list.

Here is an example list comprehension:

`[ x^2 | x <- [1..10]]`

* The `x^2` bit is the function to be applied to the input elements to generate a new list.
* The `|` seperates the output from the input. It can be read as "such that", e.g., "x squared such that..."
* The `x <- [1..10]]` specifies the input set. It says, for an input list of 1 to 10, take (`<-`) each element, bind it to `x`, and use it as an input in the output function.
* The whole comprehension could be read as "x squared, such that x is drawn from the set of numbers from 1 to 10."

### 12/7/2017 (Thursday)

Exhausted. :(

### 12/8/2017 (Friday)

**Pages 313.5-322 (8.5 pages)**

#### Adding predicates

You can add _predicates_ to list comprehensions to filter the input set. Example:

```
[ x^2 | x <- [1..10], rem x 2 == 0 ]
```

The `rem x 2 == 0` is the predicate. Note that predicates must always evaluate to Bool values.

It is also possible to use multiple generators (input lists, remember). Example:
```
[x^y | x <- [1..5], y <- [2, 3]]
```

**Note**: Generators are exhausted from **right to left**. This means that the above comprehension produces the following output:
```
[1, 1, 4, 8, 9, 27, 16, 64, 25, 125]
```

**We grab the _first input_** (in the above case, `x`) **from the leftmost generator, then we _exhaust_ the rightmost generators** before moving onto the next input from the leftmost generator.

You can add predicates to comprehensions with multiple generators simply by adding them afterwards with a comma, e.g., in `[(x, y) | x <- [1,2,3], y <- [4,5,6], x + y < 10]`, `x + y < 10` is a predicate.

**Exercises: Comprehend Thy Lists (pp.315-316)**

Number 1: `[4, 16, 36, 64, 100]` ♫

Number 2:
```
[
    (1,64), (1,81), (1,100),
    (4,64), (4,81), (4,100),
    (9,64), (9,81), (9,100),
    (16,64), (16,81), (16,100),
    (25,64), (25,81), (25,100),
    (36,64), (36,81), (36,100),
    (49,64), (49,81), (49,100)
]
```
♫

Number 3: `[(1,64), (1, 81), (1, 100), (4,64), (4,81)]` ♫

---

#### List comprehensions with Strings

* Strings ([Char]) can also be used in list comprehensions.
* `elem` is a function used to check if an element is in a list.

You can, of course, use list comprehensions in a function. Here's a function which accepts a String and returns its acronym:

```
acro xs = [x | x <- xs, elem x ['A'..'Z']]
```

Note the use of `xs`. This is both a function parameter and the generator for the list comprehension.

* In Haskell, plural variable names for lists is idiomatic.

I think the `myString` function given will return a string with all of its consonants removed. ♫

**Exercises: Square Cube (pp.317-318)**

Number 1:
```
[ (x, y) | x <- mySqr, y <- myCube ]
```
♫

Number 2:
```
[ (x, y) | x <- mySqr, y <- myCube, x < 50, y < 50 ]
```
♫

Number 3:
```
length [ (x, y) | x <- mySqr, y <- myCube, x < 50, y < 50 ]
```
♫

#### Spines and nonstrict evaluation

* **Cons cells** look like this: `a : [a]`
* A **spine** is the connective structure that ties values of a particular data structure together.

In the case of a list, the spine is textually represented by the cons (:) operator.

We can represent lists as a tree (see the book, page 318).

* **You can evaluate the spine of a list without evaluating individual values**.
* You may also evaluate only part of the spine of a list.
* In Haskell, lists aren't constructed until they're consumed. **Nothing is evaluated until it _must_ be**.
* You can use the `:sprint` command in GHCi to print variables and see what's been evaluated. Things that haven't been evaluated are represented by the underscore. See page 320 for example usage of `:sprint`.
* The `length` function only forces evaluation of the spine of a list, _not_ of its values, despite what :sprint might say.

#### Spines are evaluated independently of values

* Values in Haskell are reduced to _weak head normal form_.
* **Weak head normal form (WHNF)** means that an expression is evaluated enough to produce a desired result; it is not necessarily _fully_ evaluated.
* Normal form (fully evaluated) expressions are a subset of weak head normal form expressions.

### 12/9/2017 (Saturday)

**Pages 322-326.5 (4.5 pages)**

* `(1, 2)` is in WHNF and NF. This value is fully evaluated, which means it's in NF, which by definition means it's also in WHNF.
* `(1, 1 + 1)` is in WHNF. WHNF means that an expression is evaluated up to its first data constructor, at the very least.
* `\x -> x * 10` is in WHNF and NF. There is nothing further to reduce.
* `"Papu" ++ "chon" is _neither_.
* `(1, "Papu" ++ "chon")` is in WHNF.
* It is possible for a function to be **spine strict**, in which they case they force evaluation of the entire spine of a structure, but none of its values. One such function is `length`.
* Lists constructed with ranges are in WHNF because the compiler evaluates the head of the list but not any of the values. In other words, it evaluates the first cons constructor.
* **Pattern matching is strict by default**.
* It is possible to make a function spine strict or completely strict or neither with pattern matching, depending on how you implement it.
* **When you use underscore (`_`) to ignore pattern-matched values**, you're _not_ binding a value to underscore. In fact, that's impossible. **What you're really doing is saying to the compiler "I don't need this value, _so don't evaluate it_."**
* Spine-strict functions will work with structures that contain _bottom_ as long as the spine is defined. THey will not work if part of the spine is _bottom_.
* A list-summing function is an example of a fully strict function, because it evaluates both the spine and the values in the list.

### 12/10/2017 (Sunday)

**Pages 326.5-333.5 (7 pages)**

**Exercises: Bottom Madness (pp.326-327)**

**Will it blow up?**

1. Bottom. ♫
2. Return a value. ♫
3. Bottom. ♫
4. Return a value. ♫
5. Bottom. ♫
6. Return a value. ♫
7. Bottom. ♫
8. Return a value. ♫
9. Return a value. ♫
10. Bottom. ♫

**Intermission: Is it in normal form?**

Notes:
* **An expression cannot be in NF or WHNF if its outermost part is NOT a data constructor!**
* An expression cannot be in NF if any part of it is unevaluated.

1. Normal form.
2. WHNF.
3. Neither.
4. Neither.
5. Neither.
6. Neither.
7. WHNF.

---

#### Transforming lists of values

* In Haskell, you will generally use HOFs rather than manually writing recursive functions. This is partly because of nonstrict evaluation.
* There are two functions which apply a function to every element of a list: `map` and `fmap`. The former, `map`, only works with lists, while the latter, `fmap`, works with any Functor (to be explained later).
* **`map` and `fmap` do not apply their function argument to their list argument immediately**. The function is only applied to the values to which you force it to be applied.
* In Haskell, you can choose to apply laziness and strictness to the spine and leaves of a data structure independently.
* For performance-sensitive code: **lazy in the spine, strict in the leaves**.

### 12/11/2017 (Monday)

Production hotfix, didn't have time to study :(

### 12/12/2017 (Tuesday)

**Pages 333.5-340.5 (7 pages)**

**Exercises: More Bottoms (pp.333-334)**

1. Bottom. ♫
2. Returns a value (`[2]`). ♫
3. Bottom. ♫
4. This function takes a String and returns a list of Bools indicating which characters were vowels. Its type is `String -> [Bool]`. ♫
5. Answers below:
    1. `[1, 4, 9, 16, 25, 36, 49, 64, 81, 100]` ♫
    2. `[1, 10, 20]` ♫
    3. `[15, 15, 15]` ♫
6. `negateThrees = map (\x -> bool x (-x) (x == 3))` ♫

---

#### Filtering lists of values

* The `filter` function allows you to filter out from a list values for which a given predicate returns false. It creates a _new list_; it does _not_ mutate the original list (just like `map`).

**Exercises: Filtering (p.336)**

1. `multiplesOfThree = filter (\x -> (mod x 3) == 0)` ♫
2. `length . multiplesOfThree` ♫
3. `myFilter = filter (not . (flip elem) ["the", "a", "an"]) . words` ♫

---

#### Zipping lists

* **Zipping** functions allow you to combine lists together.
* The `zip` function combines two lists together by creating pairs of their values. It stops combining at the end of the shortest list.
* The `unzip` function is the inverse to `zip`: it extracts the original lists from a zipped list and returns them as a pair. _This won't always produce the same input lists that were passed to the zip function_, because, as we saw, `zip` stops combining at the end of the shortest list and discards the rest.
* The `zipWith` function gives you the ability to arbitrarily combine two lists using a combiner function. Its type is `zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]`. This function also stops combining at the end of the shortest list.

**Zipping exercises (p.338)**

See `chapter9_zippingExercises.hs`.

♫♫♫

---

**Chapter 9 Exercises (pp.339-344)**

**Data.Char**

See `chapter9exercises_dataChar.hs`.

♫♫♫

### 12/12/2017 (Wednesday)

**Pages 340.5-345 (4.5 pages)**

**Ciphers**

See `cipher.hs`.

**Writing your own standard functions**

See `chapter9exercises_standardFunctions.hs`.

♫♫♫

Woofah. Good job!

### 12/14/2017 (Thursday)

**Pages 345-349 (4 pages)**

#### Definitions

* A **product type** is a type whose values contain multiple, _conjoined_ values of different types. Product types are represented using data constructors with more than one arguments.
* A **sum type** is a type whose value is _one of_ a number of _disjoint_ values of different types. Sum types are represented using the pipe `|` in a datatype definition.
* **Cons** is a verb meaning that a list has been created by prepending a value to another list. In Haskell, the cons operator is the colon (`:`). The cons operator is a data constructor defined in the list datatype.
* A **cons cell** is a product of the types `a` and `[a]`, joined by the cons data constructor (`:`). Cons cells may be recursive, since they reference the list type constructor. In the expression `5 : (6 : 7 : [])`, `(5 : ...)`, `(6 : ...)`, and `(7 : [])` are all cons cells.
* The **spine** is the structure, or "skeleton", of a datatype which holds values. Spines do not _include_ the values in the data structure; they are merely the glue which holds them together. In the list datatype, for example, the spine is the series of nested cons operators that form the list. It is not the values that those operators join.


### Chapter 10: Folding lists

Nice work, mang!

### 12/15-12/16/2017 

Food poisoning. x.x

### 12/17/2017 (Sunday)

**Pages 349-358.5 (9.5 pages)**

#### Folds

* **Folds**, which are **catamorphisms**, are used to _reduce_ the spine of a data structure. They can be used to transform one datatype into another, for example, a List into a String.

#### Bringing you into the fold

* `foldr`, or "fold right" is the type of fold you'll most often want to use with lists.
* Folding functions have been abstracted out to work with any `Foldable` type since GHC 7.10. Before that, they were specific to lists.
* While maps apply a function to each value in a list, _folds replace the cons constructor with the given function_!

#### Recursive patterns

#### Fold right

* Fold right, or `foldr` is right associative, hence its name.
* Folds require a "zero value" that will be returned if the list being folded is empty and is used a starting value for the fold. This value is usually, but not always, an identity value for the function used with the fold. For example, if (+) is being used, then the identity value is 0.

#### How foldr evaluates

* The `(+)` function is strict in both of its arguments, so it forces evaluation when used.
* It is possible to use functions which only use their first argument as folding functions; such functions allow you to do less work because they won't force "force the fold", which happens when the second argument is evaluated.
* Evaluation in Haskell can be thought of as a "text rewriting system". For example, `foldr (+) 0 [1,2,3]` is rewritten as `(+) 1 ((+) 2 ((+) 3 0))`, which is then evaluated from innermost (which in this case, is also the _rightmost_) parenthetical expression.
* **Folding occurs in two stages: traversal and folding**.
* The **traversal** stage is where the fold recurses over the spine.
* The **folding** stage is where the applications of the folding function are actually _evaluated_.
* _All folds recurse over the spine in the same direction_. 
* The type of fold (left or right) merely determines the association of the folding function and thus how the reduction proceeds.
* "With `foldr`, the rest of our fold is an argument to the function we're folding with." If you look at the definition of foldr, this makes sense: `foldr f z (x:xs) = f x (foldr f z xs)`. The last bit in parenthesis is the _rest of the fold_, and its passed as an argument to the folding function.
* The `foldr` function, if given a function that chooses not to evaluate its second argument, will not force evaluation of all the values and the spine of the list! This means that **you can use foldr with an infinite list**, depending on what folding function you give it (the addition function, for example, will _not_ work because it's strict).
* There's no guarantee a fold of an infinite list will finish evaluating, even if you use `foldr`. It depends on the input data and the fold function.
* `length` only evaluates the spine of a list, not its values.
* `sum` evaluates both the spine and values of a list.
* The `take` function is nonstrict. It _stops_ returning values of a list when it reaches the specified limit. You can see this nonstrictness here: `sum $ take 2 $ take 4 [1,2,3,undefined]`.
* **For non-empty lists, `foldr` requires that the _first_ cons cell not be bottom** (though the first value may be).

### 12/18/2017 (Monday)

**Pages 358.5-365 (6.5 pages)**

#### Fold left

* Folds always traverse lists from the beginning of the spine to the end.
* Left folds are left associative. 
* **Left folds evaluate in the opposite direction as right folds**.

Given the application

`foldl (+) 0 [1, 2, 3]`

`foldl` associates like this:

`((0 + 1) + 2) + 3`

Notice how the zero-value is placed at the beginning, instead of the end like with foldr. THIS IS IMPORTANT!

* **Scans** are functions similar to folds which allow us to see all the intermediate stages of the folds. Example: `scanr (+) 0 [1..5]`.
* `scanr` corresponds to `foldr`. `(head $ scanr f z list) == foldr f z list)`
* `scanl` corresponds to `foldl`. `(last $ scanl f z list) == foldl f z list)`

Mind. F'd.

#### Associativity and folding

* **Think of evaluation in Haskell as _substitution_**. For example, when you `foldr` a list, you are effectively replacing the cons constructor with your folding function and the empty list with your zero value.
* The **parentheses are real**. They determine what gets evaluated first. For example, in right folding, when you end up with ```1 `f` (2 `f` (3 `f` z))```, the ```(3 `f` z)``` bit gets evaluated first, meaning evaluation proceeds inside-out. (Notice, though, that right folds _traverse_ the list from the outside-in.)
* **Evaluation order determines the result of your function**! `foldr (^) 2 [1..3]` is NOT equal to `foldl (^) 2 [1..3]`!

Fold right is defined as

```
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)
```

and fold left is defined as

```
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f acc [] = acc
foldl f acc (x:xs) = foldl f (f acc x) xs
```

### 12/20/2017 (Wednesday)

**Pages 365-372 (7 pages)**

* Remember, both `foldl` and `foldr` traverse the spine of the list from **left-to-right**. It's not possible for them to traverse it any other way!

**Exercises: Understanding Folds (pp.365-367)**

Number 1:

b) and c), because multiplication is associative. ♫

Number 2:

```
foldl (flip (*)) 1 [1..3]
-- let f = flip (*)
foldl f 1 [1..3] =>
foldl f (f 1 1) [2,3] =>
foldl f (f (f 1 1) 2) [3] =>
foldl f (f (f (f 1 1) 2) 3) [] =>
(f (f (f 1 1) 2) 3) =
(f (f 1 2) 3) =
(f 2 3) =
6
```

♫

Number 3:

c)

Number 4:

a)

Number 5:

a) `foldr (++) "" ["woot", "WOOT", "woot"]` ♫ 
b) `foldr max "" "fear is the little death"` WRONG. The accumulator needs to be a char, such as `'a'`.  
c) `foldr (&&) True [False, True]` ♫
d) `foldr (||) False [False, True]` ♫
e) `foldl (\b a -> b ++ show a) "" [1..5]` ♫, though I had to try several times
f) `foldr const 0 [1..5]` ♫
g) `foldr const 'a' "tacos"` ♫
h) `foldl (flip const) 'a' "burritos"` ♫
i) `foldl (flip const) 0 [1..5]` ♫

---

#### Unconditional spine recursion

* **Left folds will always recurse over the whole spine.**
* Left folds may still selectively evaluate the _values_ of the list.
* Left fold (`foldl`) is generally inappropriate for infinite lists or even just long lists for performance reasons.
* When you need a left fold, you should generally use `foldl'` ("fold-l-prime") because it is strict, meaning it forces both the spine _and_ its values as it traverses the spine. This strictness affords a performance boost.

#### How to write fold functions

* When writing a fold, begin by thinking about your **starting value**. This is usually the identity value for the folding function.

**Exercises: Database Processing (pp.371-373)**

### 12/21/2017 (Thursday)

**Pages 372-373.5 (1.5 pages)**

(See `chapter10exercises_databaseProcessing.hs`.)

### 12/22/2017 (Friday)

**Pages 373.5-379.5 (6 pages)**

#### Folding and evaluation

* `foldr` evaluates the innermost cons cell to the outermost.
* `foldl` evaluates the outermost cons cell to the innermost.
* IMPORTANT: `foldr f z xs == foldl (flip f) z (reverse xs)` (for finite lists).

#### Summary

**foldr**

* The rest of the fold (recursive invocation of foldr) is an argument to the folding function you pass to `foldr`. Hence, `foldr` **conditionally** traverses the spine; it will only traverse the whole spine if the folding function given requires it.
* Associates to the right.
* Works with infinite lists.
* Is a good default choice for transforming finite or infinite data structures.

**foldl**

* Unconditionally traverses the spine by self-applying. Values are only produced after the whole list has been traversed.
* Associates to the left.
* **Cannot** be used with infinite lists.
* Is practically useless; use `foldl'` instead.

#### Scans

* **Scans** are like folds but they return a list of intermediate values for every step of the fold.
* Scans are _not_ catamorphisms.
* There aren't many use-cases for scans.

Here are how scans represent the stages of folds:

```
scanr (+) 0 [1..3]
[1 + (2 + (3 + 0)), 2 + (3 + 0), 3 + 0, 0]
[6, 5, 3, 0]
```

```
scanl (+) 0 [1..3]
[0, 0 + 1, 0 + 1 + 2, 0 + 1 + 2 + 3]
[0, 1, 3, 6]
```

* `scanl` can create infinite, lazy lists!

#### Getting the fibonacci number we want

* (!!) is the indexing operator. Lists are 0-indexed.

**Scans Exercises**

See `chapter10exercises_scans.hs`.

### 12/23/2017 (Saturday)

**Pages 379.5-388 (8.5 pages)**

**Chapter 10 Exercises**

See `chapter10exercises.hs`.

Whew!

#### Definitions

* A **fold** is a higher-order function for transforming a collection of items into another structure. It accepts a folding function which determines how the transformation happens, a "zero value" which acts as a start value for the fold, and a collection to fold. Folds are _catamorphisms_.
* A **catamorphism** is a fold from one arbitrary datatype to another (or possibly the same). **Catamorphisms are not limited to lists, or even collections**. Example catamorphisms: `bool :: a -> a -> Bool -> a` (for transforming `Bool`s into `a`s), `maybe :: b -> (a -> b) -> Maybe a -> b` (for transforming `Maybe a`s into `b`s).
* A **tail call** is the last call made in a function. It produces the final result of the function. For example, in `f a = h $ g a`, `h` is the tail call.
* **Tail recursion** is a function whose tail calls are recursive invocations of the function itself. For example, `g x = g (h (x + 1))` is tail-recursive, because the outermost invocation is `g`.
    * `foldr` is _not_ tail-recursive: `foldr f z (x:xs) = f x (foldr f z xs)`
    * `foldl` _is_ tail-recursive: `foldl f z (x:xs) = foldl f (f z x) xs`

CHAPTER 10 K.O.'d! WOOOOOO!

### 12/24/2017 (Sunday - Christmas Eve!)

**Pages 388-396.5 (8.5 pages)**

### Chapter 11: Algebraic datatypes

* "A **[data]type** can be thought of as an enumeration of constructors that have zero or more arguments".

Haskell offers the following:
* Sum types
* Product types
* Product types with record syntax
* Type aliases (i.e., `String` = `[Char]`)
* Special datatypes called `newtype`s.

#### Data declarations review

* Creating custom datatypes helps you _model_  your _domain_ first before writing _computations_ to solve problems.
* Data constructors which take no arguments are called **nullary constructors**.
* Remember, a **data declaration** is the entire definition of a datatype.
* A **sum type** is a type with more than one data constructor.

#### Data and type constructors

* **Type** constructors are only used at the _type level_, in type signatures and typeclass definitions and instances. Types are _static_ and resolve at _compile time_.
* **Data** constructors are the _values_ of a given type and can be interacted with at runtime.
* Type and data constructors which take no arguments (nullary constructors) are called **constants**.

#### Type constructors and kinds

* **Kinds are the types of _types_**. They are one level of abstraction higher than types. They are represented with `*`.
* A fully-applied type is represented with the kind `*`. A type waiting to be applied to an argument is represented as `* -> *`. Notice the similarity to function type signatures.
* **You can query the signature of a _type_ constructor in GHCi with `:kind` or `:k`**.
* Constructors are dissimilar to functions in that they do not actually _compute_ anything. They are _similar_ to functions in that they can be applied to arguments to return a result (though in the case of constructors, the result is just a fully-applied value or type).

#### Data constructors and values

* It is possible for the type constructor in a datatype declaration to accept one or more type variables that _do not_ show up anywhere to the right of the `=`. For example: `data Thing a = Thingy`. These are known as **phantom** type arguments, as they "have no witness."
* The type variable(s) in the type constructor are the _same_ as those in the data constructors if they have the same name. In other words, in `data Something fishy = Somethin fishy`, `fishy` (in the type constructor) == `fishy` (in the data constructor). Note that the _runtime value_ of `fishy` can change, however.
* You can query data constructors just like functions: `:t Just`, for example.

### 12/25/2017 (Christmas!)

### 12/26/2017 (Family day)

### 12/27/2017 (Wednesday)

**Pages 396.5-404.5 (8 pages)**

**Exercises: Dog Types (pp.396-397)**

1. `Doggies` is a type constructor ♫
2. `* -> *` ♫
3. `*` ♫
4. `Num a => Doggies a` ♫
5. `Doggies Integer` ♫
6. `Doggies String` ♫
7. Both. The compiler uses whichever one makes sense in a given context. ♫
8. `doge -> DogueDeBordeaux doge` ♫
9. `DogueDeBordeaux String` ♫

---

#### What's a type and what's data?

* **Types are static and resolve at compile time**. That means that all the types in a program are known before the program even runs.
* **_Data_ are the values we operate upon at runtime**.
* **Compile time** is when your program is being compiled (or checked before execution in GHCi).
* **Runtime** is the execution of your program.
* Data constructors may accept _types_ (not values) as arguments.
* In order to write a data constructor which relies upon certain types, those types must be in scope wherever you define that data constructor. For example, in `data Vehicle = Car Make Model | Plane Airline`, the types `Make`, `Model`, and `Airline` must be in scope.

**Exercises: Vehicles (pp.399-400)**

(See `chapter11exercises_vehicles.hs`)

---

#### Data constructor arities

* **Arity is the number of arguments a function or constructor takes**.
* A function/constructor that takes no arguments is called _nullary_.
* **Nullary** is an adjective describing things pertaining to zero. It is to zero what binary is to two.
* Data constructors that take _one_ argument are **unary**.
* Data constructors that take _more_ than one argument are called **products**.
* Tuples are _anonymous products_  because they have no name.

#### What makes these datatypes algebraic?

* Algebraic datatypes in Haskell are algebraic because their argument structuures can be understood in terms of two basic operations: sum and product.
* The **cardinality** of a finite set is the number of unique elements in it.
* The **cardinality of a _datatype_** is the number of possible values it defines. The cardinality of `Bool`, for example, is 2.
* Knowing the cardinality of a given datatype allows us to know how many possible implementations there are for a function that consumes that datatype, and how many possible values that datatype contains.

### 12/29/2017 (Thursday - more social obligations!)

### 12/30/2017 (Friday)

**Pages 404.5-414.5 (10 pages)**

**Exercises: Cardinality (pp.404-405)**

1. 1
2. 3
3. 65,536
4. Int has a huuuuge cardinality, and Integer has an _infinite_ cardinality.
5. 256 = 2 ^ 8

---

#### Simple datatypes with nullary data constructors

**Exercises: For Example (pp.405-406)**

1. The type of `MakeExample` is `Example`. You cannot request the type of `Example`, as it _is_ a type. You can, however, request the _kind_. If you try to check the type of `Example`, you will get a "data constructor not in scope" error.
2. Yes, `:info` shows you the typeclass instances in scope for thhe given datatype.
3. The new `MakeExample` data constructor has the type `Int -> Example` because it accepts a type argument.

#### Unary constructors

* Datatypes containing only _one unary data constructor_ have the same cardinality as the type argument that constructor accepts. 
* Unary constructors are the _identity function_ for cardinality.

#### newtype

* **newtypes** are datatypes with _one_ unary data constructor. Their cardinality matches that of the type argument to the data constructor.
* newtypes have no runtime representation; they re-use the representation of the type they contain. In other words, at runtime, newtypes are identical to the type they contain.
* You can use newtypes to ensure that you don't mix types with different _domain_ representations but identical _runtime_ representations. For example, you might have a string that represents an SSN, and a string that represents a full name. You could make two newtypes for these: `newtype SSN = SSN String`, `newtype FullName = FullName String`. That way, they can't be used interchangably.
* Type synonyms and newtypes are similar in that they both equal their underlying types.
* You can define typeclass instances for newtypes, but you can't for type synonyms. 
* You can define new behavior for a type under a newtype by defining typeclass instances.
* It's possible to reuse typeclass instances from a newtype's type by turning on a language extension called GeneralizedNewtypeDeriving. 

**Exercises: Logic Goats (pp.410-411)**

(See `chapter11exercises_logicGoats.hs`.)

#### Sum types

* The pipe in sum types stands for logical disjunction, or _or_.
* To determine the cardinality of a sum type, add the cardinalities of its data constructors. `Bool`, for example, has a cardinality of 2.

**Exercises: Pity the Bool (pp.412-413)**

1. `BigSmall` has two data constructors, `Big Bool` and `Small Bool`. These are both unary constructors, and contain Bool values. Bool has a cardinality of two, so each constructor is worth 2. 2 + 2 == 4, so the cardinality is 4.
2. The cardinality of NumberOrBool is `Numba Int8` + `BoolyBool Bool` == `256 + 2` == `258`. If you create a `Numba` with an integer larger than 127, the integer will wrap back around so that it stays within the -128..127 range. The same goes for creating a `Numba` with an integer less than -128.

#### Product types

* **"A product type's cardinality is the _product_ of the cardinalities of its inhabitants."**
* Remember, product types represent logical _conjunction_, or _and_.
* **Data constructors with two or more type arguments are products**.

### 12/30/2017 (Saturday)

**Pages 414.5-426 (11.5 pages)**

#### Record syntax

* **Records** are product types with special syntax for conveniently accessing fields in the record by name.

Record syntax looks like this:

```
data Person =
    Person { name :: String
           , age :: Int }
           deriving (Eq, Show)
```
The equivalent normal product type for this would be:
```
data Person = Person String Int deriving (Eq, Show)
```

* Each name in a record is actually an _accessor function_ which can be used to access the value under that name, given a record of that type. The type of `name` in the record above, for example, is `Person -> String`.

#### Normal form

* "**All the existing algebraic rules for products and sums apply in type systems.**"
* A **sum of products** is an expression where multiplication has been distributed over addition. For example, `2 * 3 + 2 * 5` is a sum of products; it's a reduction of `2 * (3 + 5)`.
* The **normal form** of a datatype is when it is written as a sum of products (if it can be).

**Exercises: How Does Your Garden Grow?**

Number 1:

```
type Gardener = String
data Garden =
    Gardenia Gardener
  | Daisy Gardener
  | Rose Gardener
  | Lilac Gardener
```

---

#### Constructing and deconstructing values

There are **two** things we can do with a value:
* Generate/construct it
* Match on it (potentially deconstructing it) and consume it

* Data in Haskell is immutable!

**Sum and Product**

* Product types are nestable. For example, if a product type takes two type arguments, you could pass another product type as one (or both) of those arguments.

**Constructing values**

### 12/31/2017 (Sunday)

**Pages 426-436 (10 pages)**

* With sum types, you must be careful to use the correct type when constructing a value. For example, given the type `data Sum a b = First a | Second b` and the type alias `type Sunmi = Sum String Int`, this won't work: `First 5 :: Sunmi` because you're trying to use an Int where a String is expected.
* Avoid using type synonyms if you can use a data constructor or a newtype instead. The typechecker catches semantic mistakes with the latter two, but not the former.
* Record syntax is just syntactic sugar for making field references with product types.
* You can construct a record product the same way you construct a product, i.e., by passing in arguments in the correct order: `RecordProduct 1 3`.
* You can also construct records with record syntax, like so:
```
myRecord :: RecordProduct Integer Float
myRecord = RecordProduct
  { pfirst = 42, psecond = 0.5 }
```

**Exercise: Programmers (pp.430-431)**

(See `chapter11exercises_programmers.hs`.)

---

**Accidental bottoms from records**

* It is possible to partially construct record fields using record syntax by only assigning values to some of the fields. **DO NOT DO THIS**. This will cause a runtime error if something tries using that record!
* If you need to construct a record one piece at a time, use partial application of the data constructor instead.

**Deconstructing values**

* You can **deconstruct** datatypes to get at their contents. This is very often used in pattern matching and case expressions. For example, if you have a value of type `Maybe Int`, you can deconstruct it as either `(Just num)` or `Nothing`. 
* **REMEMBER**, type synonyms must start with a capital letter!
* You can deconstruct product records the normal way (e.g., `(Product firstVal secondVal)`) or by using their accessors. Here's an example:
```
-- given the datatype
data Product a b = { firstVal :: a, secondVal :: b }

type Name = String
type IsJuicin = Bool
type SportsPlayer = Product Name IsJuicin

-- normal way
juicer :: SportsPlayer -> IsJuicin
juicer (SportsPlayer _ True) = True
juicer _ = False

-- using accessors
juicer' :: SportsPlayer -> IsJuicin
juicer' player =
  case secondVal player of
    True -> True
    _ -> False

-- The above two are just for demonstration. Normally,
-- you would do this, since it's just a Bool value:
juicer'' :: SportsPlayer -> IsJuicin
juicer'' = secondVal
```

**Accidental bottoms from records**

* Be VERY CAREFUL with your record types! Do not propagate bottoms through your code by constructing partial records or by creating a record type in a sum type!
* If you must use a record in a sum type, split the record out into its own datatype declaration so the type system will catch you if you try using a record accessor function on one of the other data constructors in the sum type.

#### Function type is exponential

* **The number of inhabitants of a function can be found using exponentiation.** For example, given `a -> b`, we can calculate the inhabitants with `b ^ a`.

### 1/1/2018 (Monday)

HAPPY NEW YEAR!

**Pages 436-447.5 (11.5 pages)**

* Example: given a function `Bool -> Bool`, the number of inhabitants of the function is 2^2. Given a function from `Bool -> ThreeInhabitants` (where `ThreeInhabitants` is a type with three inhabitants), the number is 3^2 (b ^ a).
* In general, for a function `a -> b -> ... -> z`, the number of inhabitants is calculated like so, exponentiating left-to-right: `((z ^ ...) ^ b) ^ a`. Example: `a -> b -> c` => `(c ^ b) ^ a`.
* The type of functions (->) is the exponentiation operator.
* Remember: You determine the cardinality of a sum type with addition and the cardinality of a product type with multiplication.

**Exponentiation in what order?**

Write out all 2^3 (8) possible implementations of the following function: `convert :: Quantum -> Bool`.

```
-- 1
convert Yes = True
convert No = True
convert Both = True
-- 2
convert Yes = False
convert No = True
convert Both = True
-- 3
convert Yes = True
convert No = False
convert Both = True
-- 4
convert Yes = True
convert No = True
convert Both = False
-- 5
convert Yes = True
convert No = False
convert Both = False
-- 6
convert Yes = False
convert No = True
convert Both = False
-- 7
convert Yes = False
convert No = False
convert Both = True
-- 8
convert Yes = False
convert No = False
convert Both = False
```

**Exercises: The Quad (pp.440-441)**

Number 1:  
(1 + 1 + 1 + 1) + (1 + 1 + 1 + 1) = 4 + 4 = 8.

Number 2:  
4 * 4 = 16.

Number 3:  
4 ^ 4 = 256.

Number 4:  
2 * 2 * 2 = 8.

Number 5:  
(2 ^ 2) ^ 2 = 16.

Number 6:  
(4 ^ 4) ^ 2 = 65,536.

---

#### Higher-kinded datatypes

* A **higher-kinded type** is a type which accepts one or more type arguments. Its kind is something like `* -> ... -> *`.
* It is a convention in Haskell to not constrain polymorphic type variables in datatype declarations. Such constraints are the responsibility of functions that consume the datatype.

#### Lists are polymorphic

* Lists are polymorphic because they can contain values of _any_ type.

**Infix type and data constructors**

* Operators with non-alphanumeric names are infix by default.
* **Data constructors with non-alphanumeric names are also infix by default**!
* **All infix data constructors _must_ start with a colon!**
* Any operator that starts with a colon must be an infix type or data constructor. The function type constructor (->) is the only infix type constructor thhat doesn't start with a colon.
* **Infix type constructors are not allowed by default**. There is a language pragma `TypeOperators` that turns them on, however. Quote from the Haskell report:
> As with data constructors, the names of type constructors start with uppercase letters. Unlike data constructors, infix type constructors are not allowed (other than (->)). 


#### Binary Tree

The BinaryTree datatype is defined as follows:
```
data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)
```

**Inserting into trees**

* In binary search trees, data is usually ordered with the following convention: left lesser, right greater.

### 1/2/2018 (Tuesday)

**Pages 447.5-452 (4.5 pages)**

* Remember that data is immutable in Haskell. When you insert a node into a tree, you are creating a brand new tree with that node in it.

**Exercise: Write map for BinaryTree (pp.448-450)**

(See `chapter11exercises_mapTree.hs`.)

**Exercise: Convert binary trees to lists (pp.450-451)**

(See `chapter11exercises_binTreeToList.hs`.)

**Exercise: Write foldr for BinaryTree (p.452)**

(See `chapter11exercises_foldBinaryTrees.hs`.)

### 1/3/2018 (Wednesday)

**Pages 452-453.5 (1.5 pages)**

**Chapter 11 exercises (pp.452-460)**

1. a)
2. c)
3. b)
4. c)

### 1/4/2018 (Thursday)

**Pages 453.5-456.5 (3 pages)**

**Ciphers**

(See `VigenereCipher.hs`.)

Whew.

**As-patterns**

* **As-patterns allow you to reference a deconstructed datatype**. For example, in `list@(x:xs)`, `list` refers to the entire list.

(See `chapter11exercises_asPatterns.hs`.)

**Language exercises**

(See `chapter11exercises_languageExercises.hs`.)

### 1/5-1/7/18 

Rough times.

### 1/8/2018 (Monday)

**Pages 456.5-457.5 (1 page. These exercises are wrecking me!)**

**Phone exercise**

(See `chapter11exerises_phoneExercise.hs`.)

### 1/11/2018 (Thursday)

**Hutton's Razor**

(See `chapter11exercises_huttonsRazor.hs`.)

WHEW.

#### Chapter 11 Definitions

1. A **datatype** is a structured construct for holding information. It can be used as an input and output for a given function. Datatypes consist of _one_ type constructor which appears in type signatures and class instances and _zero or more_ data constructors, each of which take _zero or more_ arguments. Datatype declarations begin with the keyword `data`.

**Stopping point: p.462.**

### 1/12/2018 (Friday)

### Chapter 12: Signaling adversity

* Sometimes, you have a case where there is no reasonable value to return from a function for a given input. Haskell offers some built-in data types to gracefully handle these cases.

#### How I learned to stop worrying and love Nothing

Remember Maybe:

`data Maybe a = Nothing | Just a`

This is included in the Prelude.

You can use this datatype to explicitly return `Nothing` when there is, in fact, nothing to return.

For example:

```
head :: [a] -> Maybe a
head [] = Nothing
head (x:_) = Just x
```

You can't take the head of an empty list. You _could_ throw an error, but that destroys the runtime safety of our program. The best option? Return Nothing.

**Smart constructors for datatypes**

You can also use Maybe to make, well, smart constructors for datatypes. This is convenient when datatypes don't make sense with certain
data. The type system alone won't prevent you from constructing an invalid value, which is where the smart constructors come in.
The smart constructor is just a function which accepts some data and creates a new value of your datatype IF the data meets some criteria. If
it doesn't, return `Nothing` and be on your merry way!

#### Bleating either

The problem with `Maybe` is that it gives you no context. If you use it to signal that a bad input was given to your function, the consumer of your function doesn't necessarily know _why_ that input was bad. There's another datatype that solves this problem: `Either`. It is defined as follows:

```
data Either a b = Left a | Right b
```

* Note: datatypes do _not_ require `Eq` instances to be used in pattern matching or case expressions.
* When using Either, the convention is to store the error in `Left` or the valid output in `Right`.

**Stopping point: p.470.5**

### 1/13/2018 (Saturday)

* `Either` can, of course, hold a list in its `Left` or `Right` values. Therefore, we can enumerate the reasons a value couldn't be constructed by building up a list of those reasons in `Left` and returning that. This solves the problem of only seeing one error even if there's more than one thing wrong.

#### Kinds, a thousand stars in your types

* **Higher-kinded types** (or _type constructors_) are types which accept other types as arguments. Example: `Either a b`.
* **Type constants** are types which do not accept any arguments. Example: `Bool`.

**Lifted and unlifted types**

* "The kind `*` is the kind of all standard _lifted types_".
* The kind `#` is the kind of all standard _unlifted types_.
* **Lifted types** are those which may contain _bottom_. Lifted types are represented by a pointer.
* **Unlifted types** are those which _cannot_ contain _bottom_. These are usually native machine types and "raw pointers", which are pointers that directly address memory.
* Note: the **default kind of concrete, fully-applied datatypes is `*`** (lifted type).
* The List type has a kind of `* -> *`. The only thing that's special about it is the bracket syntax which lets you do things like `[Int]` and `[5]`.

**Data constructors are functions**

* Data constructors with an arity (number of args accepted) of 1 or more behave very similarly to normal Haskell functions. They are even curried like functions, and their arguments are typechecked according to their definitions. For example, you can't pass a String to a data constructor with the type `Int -> Example`.
* Nullary data constructors (ones that don't take any args) are _not_ like functions.

**Stopping point: p.478.5**

### 1/14/2018 (Sunday)

* If you derive a `Show` instance for a datatype with a fully polymorphic type variable, the derived instance will ensure that whatever the variable is has a Show instance so it can be shown along with the data constructor it inhabits.
* You cannot hide polymorphic types from your type constructor in a datatype declaration. For example, you _cannot_ do `data Example = Example a`. You need to do `data Example a = Example a`.
* Remember, data constructors that accept arguments are very much like functions! You can even map them over a list: `fmap Just [1,2,3]`.

**Stopping point: p.480**

### 1/15/2018 (Monday)

**Chapter 12 Exercises**

All exercises for this chapter can be found in the `chapter12/exercises` folder.

Each section has a file associated with it which is named after it, for example, `determineTheKinds.hs`.

**Stopping point: p.484.5 (#4)**

### 1/16/2018 (Tuesday)

* **Anamorphisms** are the opposite of catamorphisms: they _build up_ data structures.

**Stopping point: p.487**

### 1/17/2018 (Wednesday)

**Stopping point: p.489.5 ("Finally something other than a list!")**

### 1/18/2018 (Thursday)

#### Chapter 12 Definitions

* A **higher-kinded type** is any type which accepts arguments (in other words, any type which is a type _constructor_, not a type _constant_).

Woot woot! Another chapter down.

### Chapter 13: Building projects

**Stopping point: p.494.5 ("Making packages with Stack")**

### 1/19/2018 (Friday)

#### Making packages with Stack

* The Haskell Cabal is a package manager.
* Packages are programs, including their modules and dependencies.
* Stack is a cross-platform program for developing Haskell projects. It can be used to manage projects made up of one or more packages.
* Stack is built on top of Cabal.

#### Working with a basic project

**Stopping point: p.495.5**

### 1/20/2018 (Saturday)

* The `stack.yaml` file specifies dependency versions and what version of GHC they'll work best with.
* The `stack.yaml` file also specifies an LTS snapshot, which is a snapshot of a GHC version and Haskell packages on Stackage that are guaranteed to work together.
* Running `stack ghci` in a stack project allows you to both load in your project code and to use your project's dependencies.

**stack exec**

* You can use `stack exec` to execute a binary built by Stack in a project.

**Executable stanzas in Cabal files**

* Stack will create executables based on your cabal file. You can add an "executable stanza" to your cabal file which looks like this:

```
 executable hello
    hs-source-dirs:      src
    main-is:             Main.hs
    default-language:    Haskell2010
    build-depends:       base >= 4.7 && < 5
```

In the above config, `hello` is the name of the executable.
The `build-depends` line is where you specify your project's dependencies. In this example, the only dependency is the base library.

* Note: module names must match filenames. The compiler expects your program to be run from a Main module in a Main.hs file.
* Specifying an executable makes sense if you want your project to be run as a typical computer program. If you want your project to be used as a library, you need to add a _library stanza_, which allows you to specify which modules you want to expose from your library.

#### Making our project a library

This is what a library stanza in a cabal file looks like:

```
library
  hs-source-dirs:      src
  exposed-modules:     Hello
  build-depends:       base >= 4.7 && < 5
  default-language:    Haskell2010
```

### Module exports

* By default, when you don't explicitly specify exports for a module, it will export every top-level binding. When you import that module (without specifying an import list), all those bindings will come into scope.

Export lists look like this:

```
module Foo (func1, func2, func3, ...) where
```

Note that it is possible to have _empty_ export lists.

**Stopping point: p.501 ("Exposing modules")**

### 1/21/2018 (Sunday)

**Exposing modules**

#### More on importing modules

* Order of imports does not matter
* The `:browse` command in GHCi allows you to view what functions are available for a given named module. Example: `:browse Data.Bool`
* You can enable language extensions in GHCi by using the `--ghci-options` parameter. Example: `stack ghci --ghci-options -XNoImplicitPrelude`
* You can **selectively import things from modules** by using the following syntax: `import ModuleName (func1, func2, ...)`.

**Qualified imports**

**Qualified imports** give you a way to import the entities in a module under a specific name.  
Examples:

`import qualified Data.Bool` <-- now you must reference entities in Data.Bool by prefixing them with `Data.Bool.`, for example: `Data.Bool.bool`.

`import qualified Data.Bool as B` <-- now you must prefix entities from Data.Bool with `B.`, like `B.bool`.

* **Imports must appear at the top of your module, before any application code**.

**Setting the Prelude prompt**

* Remember, you can use `:set` to set the prompt to whatever you want.

**Intermission: Check your understanding**

1. `forever`, `when`
2. `Data.Bits`, `Database.Blacktip.Types`
3. Datatype declarations.
4. a) `MV`: `Control.Concurrent.MVar`, `FPC`: `Filesystem.Path.CurrentOS`, `CC`: `Control.Concurrent`. b) `Filesystem`. c) `Control.Monad`

#### Making our program interactive

* `do` syntax is syntactic sugar for sequencing monadic actions.
* The left arrow `<-` in `do` blocks is pronounced "bind."

#### do syntax and IO

**Stopping point: p.511**

* The `main` executable function in a Haskell program must _always_ have the type `IO ()`.
* In `do` blocks, the `<-` binds a name to the `a` in the `m a` returned by some function. In `m a`, `m` is a _monadic structure_, and `a` is the thing that inhabits it. For example, in `str <- getLine`, we're binding the name `str` to the `String` value in the `IO String` returned by `getLine`. Note that nothing is mutated here. Data is immutable in Haskell.

**return**

* The **return** function wraps a value in a monadic structure and returns it. For example, if you are in a `do` block of a function whose return value is `IO String`, then you could return a string wrapped in `IO` with `return "MyString"`.
* It is generally considered bad style to use `do` notation for single-line expressions. There is an alternative (>>=) to using `do` that you will learn about in a later chapter.

#### Hangman game

**Stopping point: p.516**

### 1/23/2018 - break day

### 1/24/2018 (Wednesday)

#### Step One: Importing modules

* You can assert the type of a function the same way you would anything else. For example: `> all :: (a -> Bool) -> Maybe a -> Bool` forces `all` to have the given type. There are limits to this, obviously. You can't assert a completely different type than what the compiler expects the function to have, but you _can_ narrow the type down.
* The `base` library that comes with your GHC install includes several different libraries, including the Prelude, Data.List, Control.Monad, and more.
* You can use the `exitSuccess` function to exit a program successfully.
* The `forever` function executes a function infinitely until the program is terminated in some way.
* The `randomRIO` function from `System.Random` can be used to select a random number within a specified range.

#### Step Two: Generating a word list

(See `hangman/src/Main.hs`)

**Stopping point: p.522.5 ("Step Three: Making a puzzle")**

### 1/27/2018 (Friday)

Break day.

### 1/28/2018 (Saturday)

#### Step Three: Making a puzzle

(See `hangman/src/Main.hs`)

**Stopping point: p.523.5**... :(

### 2/3/2018 (Saturday)

**Stopping point: p.529**

### 2/4/2018 (Sunday)

**Stopping point: p.532 ("Modifying code")**

### 2/12/2018 (Monday)

**Modifying code**

Number 1: 

(See `cipher.hs` for cipher exercise modifications)

Number 2:

(See `chapter13/palindromeIO.hs`)

Number 3:

(See `chapter13/palindromeIO.hs`)

Number 4:

(See `chapter13/gimmePerson.hs`)

Done with chapter 13! At long last! Woo!

**Stopping point: p.536**

### 2/14/2018 (Wednesday)

### Chapter 15: Monoid, Semigroup

* An **algebra** is a set of operations and the set of data they operate over.
* In Haskell, we can implement algebras with typeclasses; the typeclass defines the operations, and the instances define what _type_ of data can be operated over.
* Remember, a type is just a set of values.

#### Monoid

* **A monoid is a binary associative operation with an identity.**
* An **identity** value is a value which, when combined with another value, produces the other value. In the case of addition, the identity value is 0: n + 0 = n.
* **Associativity** for a given operation means that the order in which the operation is performed can be changed without changing the result of the operation. The order of the _operands_ must stay constant.
* The **Monoid typeclass** provides an identity value and a binary associative operation, just like monoids do.

#### How Monoid is defined in Haskell

* Examples of Monoids: Integers under addition and multiplication, lists under concatenation.

The Monoid typeclass is defined in Haskell as follows:

```
class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty
```

#### Examples of using Monoid

**List**

* Lists under concatenation are a good example of a Monoid. Look: `mappend [1, 2, 3] [4, 5, 6]` => `[1, 2, 3, 4, 5, 6]`.

**Stopping point: p.586.5**

### 2/19/2018

#### Why Integer doesn't have a Monoid

* Numbers don't have a `Monoid` instance because both addition and multiplication form monoids. Since a given type can only have one Monoid typeclass instance, and it's not clear which monoid to use, numbers have _no_ Monoid instance.
* There are, however, `Sum` and `Product` newtypes in the `Data.Monoid` module which wrap numbers and have appropriate Monoid instances.
* Several types, including list, have multiple possible monoids. In these cases, newtypes are used so the unique typeclass instance rule is enforced.
* Remember, a type isn't a monoid; **a type under an operation forms a monoid**. For example, _integers under addition form a monoid_.

**Why newtype?**

* The runtime representation of a newtype and the type it wraps are identical. There is no overhead, like there is with `data`-defined types.

Three reasons to use `newtype`:
1. Signaling intent; newtypes may _only_ wrap a single type and cannot grow into larger, more complex types.
2. Safety: they cannot be used interchangably like type aliases.
3. To add different typeclass instances to a type that is unchanged otherwise.

**More on Sum and Product**

* **There is an infix operator for `mappend`: `(<>)`**.

#### Why bother?

* Knowing about monoiads helps you recognize monoidal patterns.
* Providing a law-abiding typeclass means you can safely perform monoidal operations on types with instances for it.
* Monoids can be a useful abstraction for parallel data processing, especially if they're Abelian monoids.
* The **Abelian** monoid is a **commutative monoid**. This monoid guarantees that the results of the monoidal operation will not change based on the ordering of the operands.

#### Laws

* Algebras are defined by their laws.
* Laws provide us guarantees that give our programs strong, safe foundations.
* When the building blocks in our program are based on undeniable laws, our whole program becomes more stable as a result.

Monoid instances must adhere to the following laws:

1. Left identity
2. Right identity
3. Associativity
4. `mconcat = foldr mappend mempty`

**Stopping point: p.594.5**

### 2/22/2018 (Thursday)

#### Different instance, same representation

Bool has multiple possible monoids. These are represented by the `All` and `Any` newtypes.
* `All` is for logical conjunction (AND)
* `Any` is for logical disjunction (OR)

`Maybe` also has multiple monoids represented as newtypes:
* `First` returns the leftmost non-Nothing value.
* `Last` returns the rightmost non-Nothing value.

#### Reusing algebras by asking for algebras

* It is necessary to require a Monoid instance for types inhabiting a larger type if you want to write a "combining" Monoid instance.
* However, the typechecker does not require you to have a Monoid instance for phantom types, i.e., type arguments which are never used on the right side of the data declaration.
* `Maybe` has a third monoid, which is like "Maybe concatenation", and it requires that the value in the `Maybe` have a Monoid instance too. Something like `instance Monoid a => Monoid (Maybe a) where ...`

**Exercise: Optional Monoid**

(See `chapter15/src/optionalMonoid.hs`)

---

**Associativity**

* **Assossiativity** means that you can group/parenthesis the arguments of an operation different and the result will be the same. For example: `1 + (2 + 3)` == `(1 + 2) + 3`. Changing the grouping of the arguments is called **reassociation**.
* An operation is **commutative** when you can _re-order_ the arguments are the result stays the same. Addition is also commutative: `1 + 2 + 3` == `3 + 1 + 2`.
* An operation that is commutative is said to _commute_.
* Commutativity is useful when you don't want order to matter. Distributed systems, for example, use commutative monoids to ensure that the order of evaluation of their operation doesn't matter.

**Identity**

* The **identity value** for a given monoid turns its binary operation into the identity _function_. For example: 1 + 0 == id 1.
* There **cannot be an identity value without an operation**. The concept of an "identity value" hinges upon a relationship with a specific operation. You can say "0 is the identity value _for addition_", but it wouldn't make sense to say "0 is the identity value".

Remember the laws for the binary monoidal operation:
1. It must be associative.
2. It must have an identity value.

**The problem of orphan instances**

* **Orphan instances** are typeclass instances which are defined outside the modules where the typeclass and the type were written.
* Orphan instances **should be avoided at all costs!** Use newtypes if you don't own the datatype or the typeclass. If you own the datatype, write your typeclass instance in the same module as the type is defined. If you own the typeclass but not the datatype, write your instances in the same file as the typeclass is defined.

**Stopping point: p.604.5 ("Madness")**

### 2/26/2018 (Monday)

(See `chapter15/src/madness.hs`)

#### Better living through QuickCheck

* Since proving laws is difficult, we can use QuickCheck to find out if our code _probably_ obeys a given set of laws.
* In a nutshell, **QuickCheck is useful for testing that a given piece of code adheres to some constraints for arbitrary data.**

**Validating associativity with QuickCheck**

* It's possible to **bind infix names for function arguments**. Example: `myFunc (<>) a b = a <> b`

**Testing QuickCheck's patience**

etc etc etc.

**Exercise: Maybe Another Monoid**

(See `chapter15/src/maybeAnotherMonoid.hs`)

#### Semigroup

* A **Semigroup** is a type with an associative binary operation. Semigroups are similar to Monoids except they have **no identity value**.
* Semigroup is part of `base` but not `Prelude`. It defines its own version of `(<>)` so be careful when importing it and Monoid into the same module.

**Stopping point: p.613.5 ("NonEmpty, a useful datatype")**

### 2/27/2018 (Tuesday)

**NonEmpty, a useful datatype**

* **NonEmpty** is a list that cannot be empty.
* NonEmpty has a Semigroup instance but not a Monoid instance.
* Data constructors that **begin with a colon** and have only **nonalphanumeric characters** are **infix data constructors**.
* An Infix data constructor may not be used as a prefix, and a prefix data constructor may not be used as an infix.

#### Strength can be weakness

* The **strength** of an algebra is how many operations the algebra provides. 
* The greater the number of operations an algebra provides, the more you can do with an instance of that algebra without needing to know what type you're working with.
* The stronger an algebra is, though, the more unlikely it is a given datatype meets all the requirements to work with those operations. Therefore, it is often better to define weaker algebras which can be used separately or composed to form a stronger one. Why? Cases like NonEmpty. You want to be able to concatenate NonEmptys, but it's not a Monoid because it doesn't have an identity. Therefore, it is a semigroup and has a weaker algebra.
* Monoid is stronger than Semigroup because it exposes a strict superset of the operations Semigroup exposes.
* Remember, the more you can _do_ with a set of values, the fewer _types_ those values may inhabit.

#### Chapter 15 exercises

**Semigroup exercises**

See `chapter15/src/semigroupExercises.hs`

**Stopping point: p.618.5 (#4)**

### 2/28/2018 (Wednesday)

**Stopping point: p.619.9 (#9)**

* Rule of thumb for pattern matching: **anywhere you bind variables, you can pattern match**. This includes `let` and `where`! Examples:

```
-- let
let (x:xs) = [1, 2, 3, 4]
in ...

-- where
...
where
  (x:xs) = [1, 2, 3, 4]
```

#### Chapter 15 Definitions

* A **monoid** is a "set that is closed under an associative binary operation and has an identity element."
* When a set is **closed** under some operation, that operation always accepts arguments and returns a result in the same set.
* A **semigroup** is a set closed under an associative binary operation.
* **Laws** are rules which a given algebra must obey.
* An **algebra in Haskell-land** is (usually) a _type_ and one or more _operations_ that operate on that type and follow some _laws_.

### Chapter 16: Functor

#### What's a functor?

* A **functor** is a way to apply a function to the value(s) in a structure so that the structure remains unchanged but the value(s) is/are updated. This is called "mapping" a function over a structure.
* The `Functor` typeclass generalizes this mapping operation.

The Functor typeclass is defined as follows:

```
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

(Note that the `f` in the above is the _same_ `f` everywhere in the definition. It represents a type that implements the Functor typeclass.)

#### There's a lot of fmap goin' round

* You can use the `XTypeApplications` extension in GHCi like so: `:set -XTypeApplications`. This extension allows you to do... things.

#### Let's talk about f, baby

* The `f` in the Functor typeclass definition has the kind `* -> *`.
* **Each argument in the type signature for a function _must_ be a fully-applied and inhabitable type**, i.e., it must have the kind `*`.

**Shining star come into view**

* We know each argument in a function type signature must be of kind `*` because of the kind of the function type constructor: `(->) :: * -> * -> *`.

**Exercises: Be Kind (p.634)**

1. `*`
2. `b`: `* -> *`; `T`: `* -> *`
3. `* -> * -> *`

---

**A shining star for you to see**

* Because of the way the typeclass is defined, **type constants cannot be functors**. (It wouldn't make sense for them to be anyway.) Therefore, you can't make a Functor instance for Bool because its kind is `*` -- but you _can_ make an instance for `Maybe` since its kind is `* -> *`.
* A Functor instance for a type constant is just function application!

**Functor is function application**

* There is an **infix operator for fmap**: `<$>`.
* **Functor provides function application "over" datatypes**! In other words, `fmap` applies a function to some value(s) wrapped in a structure and returns the result of that application, wrapped in the same structure.

**Stopping point: p.637.5 ("A shining star for you to see what your f can truly be")**

#### Functor Laws

`Functor` instances must abide by the following two laws:
1. **Identity**: `fmap id == id`. For example: `fmap id [1,2,3] == id [1,2,3]`
2. **Composition**: `fmap (f . g) == fmap f . fmap g`. For example: `fmap ((subtract 1) . (+2)) [0,0,0] == fmap (subtract 1) . fmap (+2) $ [0,0,0]`

**Structure preservation**

 * **Remember, the Functor operation (`fmap`) PRESERVES STRUCTURE**. `fmap` will never change the structure of a datatype with which it's used.

#### The Good, the Bad, and the Ugly

* When writing a Functor instance, you should leave untouched anything in the datatype that is _not_ the final type argument to the functor `f`. Think of it as being part of the structure that the lifted function should be oblivious to.

#### Commonly used functors

Holy cow. There's a Functor instance for functions! It's the same as function composition.

**The functors are stacked and that's a fact**

* You can **compose fmap**! For example, you could do something like this: `(fmap . fmap) f nestedFunctors`, assuming `nestedFunctors` is a Functor within a Functor. Functorception! You can do it n layers deep. :mindblown:

**Stopping point: page 650 ("Wait, how does that even typecheck?")**

**Exercises: Heavy Lifting (p.656-657)**

(See `chapter16/src/heavyLifting.hs`)

---

#### Transforming the unapplied type argument

* Remember, you can partially apply type constructors to reduce their kindedness. For example, `:k Either` has kind `* -> * -> *`, but `:k Either Int` has kind `* -> *`.
* You can partially apply type constructors to type variables in instance declarations so you don't have to use a concrete type.
For example: `instance Functor (Either a) where`. Notice the `a` -- passing this gives us a type of kind `* -> *`, which is what Functor requires.
* Note that in a Functor typeclass instance for a partially applied type, like `Either a`, you can't touch the `a` in the `fmap` definition. Since `a` corresponds to the `Left` data constructor in Either, that means we can only map over the `Right` data constructor.
* In summary, the reason you can't `fmap` over both values in a tuple is that tuple has kind `* -> * -> *`, so you need to ignore the first value. This goes for any higher-kinded type with more than one type parameter that you're trying to write a Functor instance for.

**Stopping point: page 660.5 ("QuickChecking Functor instances")**

**Exercises: Instances of Func (p.663)**

(See `chapter16/src/instancesOfFunc.hs`)

#### Ignoring Possibilities

* `Either` and `Maybe` are useful because they allow you to `fmap` in a way that ignores failure cases.
  Their `fmap` implementations both only map over the success case data constructors.

**Maybe**

**Exercise: Possibly (p.666)**

(See `chapter16/src/possibly.hs`)

**Either**

**Short Exercise (pp.668-669)**

(See `chapter16/src/shortExercise.hs`)

#### A somewhat surprising functor

* Remember, **phantom** type parameters are those which are not referenced anywhere on the right side of a datatype definition.
* There is a Functor instance which ignores the function parameter to `fmap` for the `Constant` datatype. This is the only possible behavior for `fmap` for `Constant` because the second type parameter is a phantom type. The Functor for `Constant`, then, is virtually pointless, but behaves in a manner consistent with the datatype's functionality.

#### More structure, more functors

It is possible to create a datatype whose Functor instance requires another Functor instance. Check out the following datatype:

```
data Wrap f a = Wrap (f a) deriving (Eq, Show)
```

To write a Functor instance for this, `f` must also have a Functor instance so we can apply the function passed to `fmap` to `a`:

```
instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)
```

Interesting, eh?

#### IO Functor

* IO is actually an _abstract datatype_: its implementation is hidden. 
* In general, **abstract datatypes** are those which leave some or all of their representation undefined or hidden. This can be accomplished with either parameterized types ("weak" abstraction) or the module system, by only exporting the type constructor. See https://wiki.haskell.org/Abstract_data_type for more info.
* Because IO is an abstract datatype and does not expose any data constructors for us to match on, the only way we can interact with it is through its typeclass instances.

**Stopping point: page 674.**

* GHCi does not print out `IO ()` values.
* IO has a Functor instance that you can use to `fmap` over it. For example: `fmap (++ " wololo") getLine`

#### What if we want to do something different?

* **Natural transformations** are those which change a _structure_ but not the _value(s)_ it contains.
Here is a natural tranformation type:

```
{-# LANGUAGE RankNTypes #-}

type Nat f g = forall a . f a -> g a
```

The `forall a` bit lets us avoid defining `a` as a type parameter. We don't want `a` to be a type parameter in this case
because we _only_ care about the structure -- we don't want to mess with the value(s) those structures hold!
Using this "quantifier", as it is called, ensures that in our functions we _can't_ touch the `a` value, which is true to the
spirit of natural transformations.

#### Functors are unique to a datatype

* In Haskell, **for a given datatype, only one valid Functor instance exists**. (Compare to Monoids, where there may be multiple possible valid instances and we use newtypes to create separate instances.)
* We _can_ flip type arguments using the `Flip` newtype: `newtype Flip f a b = Flip (f b a)`. However, this means we are using an entirely different datatype and it can become unwieldy.

#### Chapter 16 exercises

(See `chapter16/src/chapterExercises.hs`)

**Chapter 16 Definitions**

* **Higher-kinded polymorphism** occurs when a type variable is used to represent a higher-kinded type. `Functor` is an example of higher-kinded polymorphism. Look at this snippet of the output for `:info Functor`: `class Functor (f :: * -> *) where`. Notice how `f` represents a higher-kinded type.
* **Functor** is a mapping between categories. In Haskell, Functor is a typeclass which allows you to transform the values in a structure without affecting the structure itself. Note that there is a Functor instance for functions.
* **Lifting** is the concept of raising a value into the context of a datatype. For example: `f <$> [1,2,3]` _lifts_ `f` over `[1,2,3]`.

### Chapter 17: Applicative

#### Applicative

* Applicatives are monoidal functors, because they lift functions over datatypes and join datatypes together. This is because they lift a function from one structure to another structure of the same type and apply to the values inhabiting that structure. In the process, the two structures -- both the one holding the function and the one holding values -- are combined.

#### Defining Applicative

* Functor is a superclass of Applicative.

The Applicative typeclass has two primary methods:
* `pure` - This lifts a value into an Applicative context
* `(<*>)` - Called "apply", "ap", or "tie fighter", this applies a function in an Applicative to values within an Applicative. It's similar to `fmap`, except that the function itself is also wrapped in the case of `(<*>)`.

#### Functor vs Applicative

* You can actually define a Functor in terms of an Applicative: `fmap f x = pure f <*> x`

#### Applicative functors are monoidal functors

Applicative is monoidal because we have to combine structure. Look at the `(<*>)` type signature:

`(<*>) :: f (a -> b) -> f a -> f b`

Not only do we have to apply `a` to `a -> b`, but we have to return a new value of type `f` which is a combination of the previous two values given (and it has to hold `b`). Monoid gives us this ability: `mappend :: Monoid a => a -> a -> a`, or to make it clearer, `mappend :: Monoid f => f -> f -> f`.
In this way, Applicative is monoidal as well as functorial.

**Stopping point: p.693**

**Show me the monoids**

`instance Monoid a => Applicative ((,) a)`

The above illustrates that tuples have an Applicative instance which requiers a Monoid instance.
This is necessary so you can mash the first value of the tuple together when using apply:

`("wolo", (+5)) <*> ("lo", 5)` returns
`("wololo", 10)`

See how "wolo" and "lo" were combined into one value in the return tuple? This is why the Monoid instance on the first value is necessary.
We don't need a Monoid instance for the second value because that is produced through function application.

**Tuple Monoid and Applicative side by side**

Why does the tuple Applicative require a Monoid and not just a semigroup, you  might ask? This is why:

`pure x = (mempty, x)`

Monoid provides `mempty` -- if we didn't have that, what would we fill the first slot in the tuple with?

**Maybe Monoid and Applicative**

* The monoidal part of an Applicative instance may not match the behavior of the Monoid instance defined for the same datatype. Remember that there are multiple possible valid monoids for many datatypes.

#### Applicative in use

**List Applicative**

* Type Applications are helpful for specifying types for functions explicitly. https://ghc.haskell.org/trac/ghc/wiki/TypeApplication

**What's the List applicative do?**

The list applicative applies all the functions in one list to the values in another list. The ordering is as follows:

```
> [(+1), (+2)] <*> [1, 2]
[(+1) 1, (+1) 2, (+2) 1, (+2) 2]
[2, 3, 3, 4]
```

**Stopping point: p.700**

**Exercises: Lookups (pp.702-703)**

(See `chapter17/src/lookups.hs`)

---

**Identity**

**Specializing the types**

The `Identity` type can be useful for wrapping structure in extra structure when you don't want to
operate upon the original structure. For example:

`const <$> Identity [1,2,3] <*> Identity [9,9,9]` returns `Identity [1,2,3]`

This is how `Identity` is defined:

`newtype Identity a = Identity a`

**Exercise: Identity Instance (pp.704-705)**

(See `chapter17/src/identityInstance.hs`)

**Constant**

The `Constant` datatype is defined like so:

`newtype Constant a b = Constant { getConstant :: a }`

Basically, it throws away its second type parameter.

**Exercise: Constant Instance (p.706)**

(See `chapter17/src/constantInstance.hs`)

---

**Stopping point: p.706 ("Maybe Applicative")**

**Maybe Applicative**

In the case of the Maybe Applicative, we may or may not even have a function to apply. It could be Nothing!

**Specializing the types**

**Using the Maybe Applicative**

You can use the Maybe Applicative in some cool ways. Here's an example:

```
ok :: Int -> Maybe Int
ok n = if n < 10 then Just n else Nothing

addIfOk :: Int -> Int -> Maybe Int
addIfOk n1 n2 = (+) <$> ok n1 <*> ok n2
```

See that? We can optionally apply functions over Maybes by using fmap and apply without
having to write any explicit conditional code. Very cool.

**Breaking down that example**

**Maybe Applicative and Person**

**Before we moooove on**

From the book, a good way to tell if you need to use an Applicative is when you find yourself thinking something like:

"I want to do something kinda like an fmap, but my function is embedded in the functorial structure too, not only the value I want to apply my function to."

**Exercise: Fixer Upper (p.721)**

(See `chapter17/src/fixerUpper.hs`)

**Stopping point: p.721 ("Applicative laws")**

#### Applicative laws

Here are the laws for the Applicative typeclass:

1. Identity: `pure id <*> v = v`
2. Composition: `pure (.) <*> u <*> v <*> w = u <*> (v <*> w)`
3. Homomorphism (a **homomorphism** is a structure-preserving map between two algebraic structures): `pure f <*> pure x = pure (f x)`
4. Interchange: `u <*> pure y = pure ($ y) <*> u`


**Stopping point: p.726**

#### You knew this was coming

* There is a library that goes with QuickCheck called Checkers. It makes verifying common typeclass laws via property checking easier.

#### ZipList Monoid

The ZipList monoid for lists combines lists differently than the default list monoid.  
Whereas the list monoid `mappend` concatenates two lists together, the ZipList
monoid `mappend` combines the actual values together. Example:

```
["foo", "bar"] `mappend` ["bar", "baz"]
==
["foobar", "barbaz"]
```

**Zero vs. Identity**

There's a difference between "zero" and "identity":

zero: n * 0 == 0

identity: n * 1 == n

**Stopping point: p.733 ("List Applicative Exercise")**                                                               

**List Applicative Exercise (pp.733-735)**

(See `chapter17/src/listApplicative.hs`)

**ZipList Applicative Exercise (pp.736-737)**

(See `chapter17/src/zipListApplicative.hs`)

**Stopping point: p.737 ("Either and Validation Applicative")**

**Either and Validation Applicative**

Types for Applicative functions specialized to Either:

```
(<*>) :: Either e (a -> b) -> Either e a -> Either e b
pure :: a -> Either e a
```

**Either versus Validation**

* "Often, the interesting part of an Applicative is the monoid."
* **Applicative can have more than one valid and lawful instance for a given datatype.** (Functor cannot.)

Here is the Validation datatype definition:

```
data Validation err a =
    Failure err
  | Success a
  deriving (Eq, Show)
```

Note that this is _identical_ to the Either datatype. The only reason for it is that it has a different Applicative instance.
There are a number of functions which can convert between Either and Validation.

* The **Either Applicative throws away everything but the first `Left`, if there is a Left present.**
* Contrastly, the **Validation Applicative combines the values of the `Failure` data constructors by using their Monoid instance.**

**Exercise: Variations on Either (p.740)**

(See `chapter17/src/variationsOnEither.hs`)

#### Chapter 17 exercises

Specialize the types.

Number 1:

```
pure :: a -> [a]
(<*>) :: [(a -> b) -> [a] -> [b]
```

Number 2:

```
pure :: a -> IO a
(<*>) :: IO (a -> b) -> IO a -> IO b
```

Number 3:

```
pure :: a -> (,) b a
(<*>) :: (,) z (a -> b) -> (,) z a -> (,) z b
```

Number 4:

```
pure :: a -> (->) e a
(<*>) :: (->) e (a -> b) -> (->) e a -> (->) e b
```

Write instances.

(See `chapter17/src/chapterExercises.hs`)

**Stopping point: p.742 ("Combinations")**

#### Chapter 17 Definitions

1. **Applicatives** in Haskell are _monoidal functors_. In a nutshell, Applicatives give you a way to apply functions wrapped in some structure to values wrapped in the same type of structure and produce a new structure which is the combination of the previous two with the newly produced value in it.
2. **Idiom** is another word for **applicative functor**.

### Chapter 18: Monad

#### Monad

* Monads are not strictly necessary for Haskell. Haskell defines monads; monads do not define Haskell.
* Monads are applicative functors with a little more zing that makes them more powerful.

#### Sorry -- a monad is not a burrito

* Monads are another way of applying functions over structure.

Here is the definition of the Monad typeclass:

```
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
```

Notice that Applicative is a superclass of Monad.

**Applicative m**

* Since Monad is stronger than Applicative, and Applicative is stronger than Functor, you can derive Applicative and Functor using only monadic operations. Check it out, here's functor: `fmap f xs = xs >>= return . f`. This is a law.
* Here's the class dependency chain: Functor -> Applicative -> Monad
* When you write an instance of Monad for a type, you _necessarily_ also have an Applicative and a Functor instance for that type.

**Core operations**

* `return` is the same as `pure` from Applicative.
* `>>` is called "Mr. Pointy" or _the sequencing operator_. It sequences two monadic actions together and discards the resulting value of the first action. Applicative has a similar operator.
* `>>=` is called _bind_ and is what makes Monad special.

**The novel part of Monad**

* Monad is sort of a generalization of `concat`, except for any monadic structure, not just Foldables wrapping lists.

**The unique part of Monad is this function:**

```
join :: Monad m => m (m a) -> m a
```

**Stopping point: p.749 ("It's somewhat novel...")**

**Exercise: Write bind in terms of fmap and join (p.750)**

(See `chapter18/src/fmapJoinBind.hs`)

**What Monad is not**

Don't base your understanding of monads on the IO monad. This can limit your intuitions around what a monad is and what it can do.

A monad is **not**:

* Impure. The IO datatype allows for impurity, but monads themselves are not impure.
* An embedded language for imperative programming.
* A value. This is the same for monoids and functors -- they're not values, they're algebras associated with types.
* About strictness. `bind` and `return` are nonstrict.

You don't have to know math or category theory to use monads.

"The Monad typeclass is generalized structure manipulation with some laws to make it sensible. Just like Functor and Applicative."

**Monad also lifts!**

The `Monad` module includes a set of `lift` functions that are identical in functionality to the ones from `Applicative`. These are preserved for backwards compatibility.

* `liftA` and `liftM` are the same as `fmap`, but with different typeclass constraints (`Applicative` and `Monad`, respectively).
* `liftA2` and `liftM2` allow you to apply a 2-arity function to values from two structures of the same type.
* The `zipWith` function is `liftA2` or `liftM2` specialized to the [] type. It uses a different list monoid, so it behaves differently than either of those functions.
* There are also `liftA3`, `liftM3`, and `zipWith3` functions. Same story. The first two do the same thing, the third behaves differently because it uses a different monoid.

#### Do syntax and monads

The two functions

```
(*>) :: Applicative f => f a -> f b -> f b
(>>) :: Monad m => m a -> m b -> m b
```

are essentially the same, except with different typeclass constraints. They are both used for sequencing.

**Stopping point: p.755 ("When fmap alone isn't enough")**

Doing `putStrLn <$> getLine` will not cause all the effects to be evaluated. Only `getLine` will be evaluated, but `putStrLn` will not. This is because this expression produces the type `IO (IO ())`. To make the expression work, we need to use `join` (which is the unique thing that Monad offers):

```
join $ putStrLn <$> getLine
```

IO actions (or any monadic actions) are ordered via the nesting of lambda expressions.
**Monadic syntax is still built on lambda calculus**. It's simply nested lambdas.

* The side effects of the `IO` type are constrained _to_ the IO type itself.

Here's an example of desugaring `do` syntax:

```
bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn ("y helo thar: " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
  putStrLn "name pls:" >>
  getLine >>=
  \name ->
    putStrLn ("y helo thar: " ++ name)
```

As you can see, `do` syntax allows us to flatten out deep nesting produced by binding multiple variables.

#### Examples of Monad use

**List**

**Specializing the types**

Here's `(>>=)` specialized to the list type:

`(>>=) :: [a] -> (a -> [b]) -> [b]`

and `return`:

`return :: a -> [a]`

**Example of the List Monad in use**

`[1,2,3] >>= (\x -> if even x then [x*x] else [x])`

**Maybe Monad**

**Specializing the types**

**Using the Maybe Monad**

See `chapter18/src/usingTheMaybeMonad.hs`.

**Stopping point: p.764 ("Can we write it with (>>=)?")**

See `chapter18/src/usingTheMaybeMonad.hs`.
See `chapter18/src/doSomething.hs`.

The following cannot be rewritten using Applicative:
```
doSomething' n = do
  a <- f n
  b <- g a
  c <- h b
  pure (a, b, c)
```
The reason for this is because each subsequent calculation relies on a value produced in a previous calculation.
Monad allows you to create a sort of "value pipeline" like this, but Applicative does not: each calculation stands alone.

**Exploding a spherical cow**

* (>>=) is sort of like a "plunger" that lets you force the value(s) out of their structure and into a function which produces a new structure of the same type with values of a different or the same type.

**Fail fast, like an overfunded startup**

The Maybe Monad implementation of `(>>=)` returns Nothing without evaluating the given function if the first argument is Nothing.
I.e., `Nothing >>= _ = Nothing`.

You can explicitly write out a chain of Maybe computations identical to a monadic `do` block using `case`s, but it's much cleaner and more concise to use the Monad.

**Either**

**Specializing the types**

Here is how the types of the monadic operations are specialized for Either:

```
(>>=) :: Either e a -> (a -> Either e b) -> Either e b
return :: a -> Either e a
```

**Using the Either Monad**

* The `Either` Monad short-circuits and fails as soon as a `Left` value is encountered.
* The `Validation` type does not have a Monad instance, because it has a different Applicative instance and the only possible Monad you could write has the same behavior as `Either`'s Monad instance, _which relies on a different Applicative_. Remember, Applicative and Monad must have the same behavior -- you must be able to derive `(<*>)` from `(>>=)`.

**Stopping point: p.776**

**Short Exercise: Either Monad**

(See `chapter18/src/eitherMonad.hs`.)

#### Monad laws

**Identity laws**

Monad has two identity laws:

```
-- right identity
m >>= return = m

-- left identity
return x >>= f = f x
```

In a nutshell, `return` should not perform any computations.

**Associativity**

```
(m >>= f) >>= g = m >>= (\x -> f x >>= g)
```

The reason the right side looks different is because of the left-to-right nature of bind.
We need to apply `f` to an argument so it produces an `m a` value which can be bound to `g` with `>>=`.

**We're doing that thing again**

We can use the checkers library to validate our Monad instances. Here's an example of how:

```
quickBatch (monad [(1,2,3)])
```

This might look familiar -- it's the same way we check Applicative instances (except for the use of the `monad` function, obviously).

**Bad Monads and their denizens**

* It's possible and can be easy to accidentally write an invalid Monad, Applicative, or Functor instance that typechecks but breaks the relevant laws. That's why it's important to use QuickCheck to validate your instances to be sure.
* Sometimes, you can have a valid Functor and Applicative but not a valid Monad instance.
* QuickCheck catches invalid instances that do something weird and harmful. That's useful!

#### Application and composition

* Composition "just works" for Functors and Applicatives: `fmap (+1) . fmap (*2) $ [1,1,1] == [3,3,3]`

Monads are composed using the bind operator:

```
mcomp :: Monad m =>
         (b -> m c)
      -> (a -> m b)
      ->  a -> m c
mcomp f g a = g a >>= f
```

**Kleisli composition** allows you to compose monads. The "Kleisli fish" `(>=>)` operator is defined in Control.Monad and has the following type signature:

```
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
```

which is actually just function composition with its arguments flipped and that accounts for monadic structure. Notice the similarity:
```
flip (.) ::         (a -> b)   -> (b -> c)   -> a -> c
```

See page 786 for an example use of Kleisli composition.

#### Chapter 18 Exercises

(See `chapter18/src/chapterExercises.hs`.)

**Stopping point: p.789**

#### Chapter 18 Definitions

* **Monad** is a typeclass that provides two operations: `return` and `(>>=)`. The magic of monad is in `(>>=)`, which you can use to functorially apply a function to produce more structure for every value in a given structure then flatten that nested structure out (using `join` internally).
* A **monadic function** is one which generates _more_ structure after having been lifted over monadic structure.
* In the context of Monads, **bind** refers to using the `(>>=)` operator to lift a monadic function over a structure. Outside of a monadic context, bind just means binding variables to values. "Binding over" can mean using `<-` in do-notation or using `(>>=)`.

Woo! Done with chapter 18! :D

### Chapter 19: Applied Structure

This chapter will be a bunch of examples of practical uses of Monoid, Functor, Applicative, and Monad.

#### Monoid

**Templating content in Scotty**

* Scotty is a web framework for Haskell.

**Concatenating connection parameters**

Aditya Bhargava wrote a blogpost called "Making A Website With Haskell" which may be a useful read.

**Concatenating key configurations**

* `xmonad` is a windowing system for X11 written in Haskell.
* The exclamation point (`!`), when used in type signatures, is a **strictness annotation**. More on those later in the book.

The Monoid of functions looks like this: 

`instance Monoid b => Monoid (a -> b)`

This means that you can `mappend` the results of two function applications, because they share a Monoid instance.
(It also means that you can mappend unapplied functions together too, like `f <> g`.)

**Stopping point: p.797**

#### Functor

**Lifting over IO**

**Lifting over web app monads**

* When writing web applications, you'll often have a monad which describes the application. This Monad will have a type parameter that describes what result was produced in the course of a running web application.

#### Applicative

Applicative has many uses in parsers.

**hgrev**

**More parsing**

**Stopping point: p.803 ("This one uses `liftA2`...")**

**And now for something different**

You can lift operators over structure as well. Check this out:

```
(<||>) :: f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)
```

Here are some ways to use it:
```
> (Just True) <||> (Just False)
Just True
> (\a -> True) <||> (\b -> False) $ 5
True
```

Neat, huh?

#### Monad

* In Haskell, effectful programming is constrained to the IO datatype.
* IO has an instance of Monad, so practical uses of Monad can be found all over the place.

**Opening a network socket**

**Binding over failure in initialization**

#### An end-to-end example: URL shortener

**Stopping point: p.808**

* The `OverloadedStrings` language extension makes string literals polymorphic so they can be Text, String, or ByteString values.

**Brief aside about polymorphic literals**

The way OverloadedStrings works is by using the `IsString` typeclass, which has a `fromString` method, similar to how `Num` has a `fromInteger` method and `Fractional` has `fromRational`.

In a nutshell, GHC treats string literals as being concrete `String`s, but under the hood wraps them in `fromString` calls so they can be polymorphic. (The same is true for integral and fractional literals.)

**Back to the show**

* There are various ways to import things from modules. You can find a full explanation of these ways here: https://wiki.haskell.org/Import.
* It's good practice to make your imports as explicit as possible, using either qualified (and named) imports and/or specific imports (e.g., `import Foo (x, y)`.

**Exercise (p.820)**

Skipping this exercise because I'm running on a Windows machine and Redis is for unix boxes. There's a windows clone, sure, but... ugh.

#### That's a wrap!

The next two chapters, Foldable and Traversable, cover typeclasses that rely on Monoid, Functor, Applicative, and Monad.

### Chapter 20: Foldable

**Stopping point: p.823**

#### Foldable

* Folding is monoidal in nature.

#### The Foldable class

* The minimal complete definition for a Foldable instance requires you to write either a `foldMap` function or `foldr` function.
* Foldable requires a datatype with the following kind: `* -> *`. Just like Functor.

#### Revenge of the monoids

Folding implies a monoid. The Foldable typeclass makes this explicit with some of its operations:

```
fold :: Monoid m => t m -> m
foldMap :: Monoid m => (a -> m) -> t a -> m
```

Note that `fold` just uses the Monoid instance of the element(s) inhabiting `t` to produce a summary value.

Holy cow. You can use type annotations to force a list of numbers into a specific newtype, like so:

```
xs :: [Sum Integer]
xs = [1, 2, 3, 4, 5]

-- and
ys :: [Product Integer]
ys = [1, 2, 3, 4, 5]
```

**And now for something different**

`foldMap` is like `fold`, except that it requires you to first map each element in the structure to a Monoid first.

Note that if you `foldMap` over a structure that already contains a monoidal value, then the function you pass doesn't necessarily have to return a NEW type of monoidal value. For example:

```
> foldMap id [Sum 1, Sum 2, Sum 3]
Sum {getSum = 6}
```

Compare this to `foldr`, where the only Monoid that matters is the one implied by the folding function. Your input Foldable could be any Monoid, but it wouldn't matter -- only the folding function matters.

When you foldMap over a `Foldable` that only contains one value (like Maybe), it doesn't "need" the Monoid instance to combine values, because there's only one value. However, you still need a Monoid instance to make it typecheck, and `mempty` is used in the event no value is present (e.g., folding over `Nothing`).

**Stopping point: p.828 ("20.4 Demonstrating Foldable instances")**

#### Demonstrating Foldable instances

**Identity**

**Maybe**

* The `Foldable` instance for Maybe relies on the given type's monoidal `mempty` for `foldMap` in the event of Nothing.

#### Some basic derived operations

Here's the info on the `Foldable` typeclass:

```
class Foldable (t :: \* -> \*) where
  Data.Foldable.fold :: Monoid m => t m -> m
  foldMap :: Monoid m => (a -> m) -> t a -> m
  foldr :: (a -> b -> b) -> b -> t a -> b
  Data.Foldable.foldr' :: (a -> b -> b) -> b -> t a -> b
  foldl :: (b -> a -> b) -> b -> t a -> b
  Data.Foldable.foldl' :: (b -> a -> b) -> b -> t a -> b
  foldr1 :: (a -> a -> a) -> t a -> a
  foldl1 :: (a -> a -> a) -> t a -> a
  Data.Foldable.toList :: t a -> [a]
  null :: t a -> Bool
  length :: t a -> Int
  elem :: Eq a => a -> t a -> Bool
  maximum :: Ord a => t a -> a
  minimum :: Ord a => t a -> a
  sum :: Num a => t a -> a
  product :: Num a => t a -> a
  {-# MINIMAL foldMap | foldr #-}
```

Remember that all of the operations can be defined in terms of `foldMap` or `foldr`.

* `toList` simply puts all the elements in a structure into a list. In the case of Nothing, for example, it returns `[]`.
* `null` checks whether a given structure is empty. It returns true for Left, Nothing, empty lists, and so on.
* `length` gives the number of `a` elements in a structure. Note that this doesn't include elements which are "part of the structure", e.g., `length (Left 5)` == `0`
* `elem` determines if an element is in a structure (only checking the `a` values, of course).
* `maximum` gives the largest element a non-empty structure, and `minimum` the smallest. Be careful: these will throw runtime errors if given empty structure.
* `sum` and `product` well... give the sum and product of the members of a structure. If the structure is empty, the appropriate identity value will be returned.

**Stopping point: p.835 ("Exercises: Library Functions")**

**Exercises: Library Functions (p.835-836)**

(See `chapter20/src/libraryFunctions.hs`.)

#### Chapter 20 Exercises

(See `chapter20/src/chapterExercises.hs`.)

Done with chapter 20! Woot!

### Chapter 21: Traversable

**Stopping point: p.839**

#### Traversable

Note the first line of `:info` about the Traversable class:

`class (Functor t, Foldable t) => Traversable (t :: \* -> \*)`

Functor and Foldable are superclasses of Traversable.

Traversable gives you a way to "traverse a data structure, mapping a function inside a structure while accumulating the applicative contexts along the way."

#### The Traversable typeclass definition

The `Traverse` typeclass has two primary functions:

* `traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)`
* `sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)`

These two functions can be defined in terms of each other, so minimally, you need to define one of them for your instance.

#### sequenceA

The `sequenceA` function allows you to flip two structures around. For example:

```
> sequenceA $ Just [1,2,3]
[Just 1, Just 2, Just 3]
> sequenceA $ [Just 5, Nothing]
Nothing
```

Note that `Data.Maybe` offers a `catMaybes` function which takes a list of Maybe values and returns a new list containing only the values from `Just`s in the original list:

```
> catMaybes [Just 1, Just 2, Nothing]
[1,2]
```

#### traverse

This is how `traverse` is defined by default:

`traverse f = sequenceA . fmap f`

Here's an example of how it works:

```
> traverse Just [1,2,3]
Just [1,2,3]
```

**mapM is `traverse`**

**In versions of GHC prior to 7.10**, `mapM` is basically a more concrete version of `traverse`, and `sequence` is a more concrete version of `sequenceA`.

#### So, what's Traversable for?

Well, anytime you wanna invert a structure of two nested type constructors, then that's Traversable. Also, anytime you wanna map a function over a doubly-nested structure then invert it, that's Traversable.

**Stopping point: p.845 ("Morse code revisited")**

#### Morse code revisited

`traverse` is just `fmap` and `sequence`. `sequence` is what makes it special

#### Do all the things

**Stopping point: p.851 ("Strength for understanding")**

**Strength for understanding**

Since Traversable is stronger than Functor and Foldable, we can implement those classes in terms of Traversable's operations.

#### Traversable instances

**Either**

**Tuple**

#### Traversable Laws

`traverse` must satisfy the following laws:

* Naturality: `t . traverse f = traverse (t . f)`
* Identity: `traverse Identity = Identity`
* Composition: `traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f`

`sequenceA` must satisfy these laws:

* Naturality: `t . sequenceA = sequenceA . fmap t`
* Identity: `sequenceA . fmap Identity = Identity`
* Composition: `sequenceA . fmap Compose = Compose . fmap sequenceA . sequenceA`

#### Quality Control

* You can QuickCheck Traversable instances with the checkers library.

#### Chapter 21 Exercises

In `chapter21/src`, see `traversableInstances.hs`, `skiFree.hs`, and `instancesForTree.hs`.

Done with chapter 21! Noice!

### Chapter 22: Reader

#### Reader

* Reader is a solution to the problem of having to pass around information that needs to be accessed by all parts of an application.

#### A new beginning

This section will use `chapter22/src/examples.hs`.

Remember that, when you load code from a file, the monomorphism restriction applies, which forces any un-annotated bindings to have concrete types.

`fmap` for functions is the same as composition. This makes sense because of the following:

```
fmap (+1) (*5)
-- is equivalent to
\x -> (+1) $ (*5) x
```

**Stopping point: p.863 ("Now we're in an Applicative context.")**

* You can use `(<*>)` from the Applicative instance for functions to apply two functions to the same argument and combine their results with a third function. For example: `((+) <$> (*2) <*> (+10)) 3` == `(3*2) + (3+10)` == `19`
* The type of `(<*>)` for functions is `(<*>) :: (a -> a -> b) -> (a -> a) -> (a -> b)`
* Functions even have an instance of Monad! See `boopDoop` for an example.
* The idea of Reader is to string functions together that share an input.

**Short Exercise: Warming Up (pp.867-868)**

(See `chapter22/src/warmingUp.hs`.)

#### This is Reader

* Reader utilizes the instance of Monad for functions. In fact, when we refer to "Reader", we are sometimes referring to the practice of using the function applicative and monad.
* Reader, e.g., the function Monad, allows us to read an argument from the environment into functions.

#### Breaking down the Functor of functions

* The `r` in `((->) r)` stands for "reader".
* Since functions have the kind `* -> * -> *`, and Functor, Applicative, and Monad take `* -> *`-kinded types, those instances must ignore the first type variable in the function datatype definition, which is the function's parameter. Here's the definition: `data (->) a b`
* The **return value** of a function is the value that's transformed by `fmap`, for example, **not** the parameter.

Function composition actually matches the type of `fmap` for Functions. Check it out:

```
fmap :: Functor f => (a -> b) -> f a -> f b
     ::              (b -> c) -> f b -> f c
                     (b -> c) -> ((->) a) b -> ((->) a) c
                     (b -> c) -> (a -> b) -> (a -> c)

(.)  ::              (b -> c) -> (a -> b) -> (a -> c)
```

#### But uh, Reader?

**Reader** is a newtype wrapper for the function type:

```
newtype Reader r a =
  Reader { runReader :: r -> a }
```

Reader's Functor instance is basically identical to the function Functor instance, but with the Reader data constructor sprinkled in.

**Exercise: Ask (p.873)**

(See `chapter22/src/ask.hs`.)

#### Functions have an Applicative too

Here are the types of the Applicative operations, specialized to the function type:

```
pure :: a -> (r -> a)
(<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
```

**Demonstrating the function Applicative**

"Reader" doesn't always refer to the Reader newtype; sometimes it refers to the use of the Applicative and Monad instances of the function type.

**Stopping point: p.876**

**Exercise: Reading Comprehension (pp.877-878)**

See `chapter22/src/readingComprehension.hs`

**Stopping point: p.878 ("The Monad of functions")**
