# Haskell

<img src = "https://blog.realogs.in/content/images/2021/09/haskellbanner.png">

## Table of Contents

<div style="display: flex; flex-wrap: wrap;"> <div style="flex: 1; min-width: 200px; margin-right: 20px;"> <ul> <li><a href="#ghci-commands">GHCI commands</a></li> <li><a href="#indentation">Indentation</a></li> <li><a href="#data-types">Data Types</a></li> <li><a href="#custom-data-types">Custom Data Types</a></li> <li><a href="#type-signature">Type Signature</a></li> <li><a href="#function-parameters">Function Parameters</a></li> <li><a href="#let--in">Let .. in</a></li> <li><a href="#if-statements">If Statements</a></li> <li><a href="#guards">Guards</a></li> <li><a href="#where-clause">Where Clause</a></li> <li><a href="#recursion">Recursion</a></li></ul> </div> <div style="flex: 1; min-width: 200px;"> <ul> <li><a href="#pattern-matching">Pattern Matching</a></li> <li><a href="#basic-list-functions">Basic List Functions</a></li> <li><a href="#list-transformations">List Transformations</a></li> <li><a href="#list-operations">List Operations</a></li> <li><a href="#list-comprehension">List Comprehension</a></li> <li><a href="#zip">Zip</a></li> <li><a href="#sections">Sections</a></li> <li><a href="#lambda-expressions">Lambda Expressions</a></li> <li><a href="#operatorul-">$ Operator</a></li> <li><a href="#constraints">Constraints</a></li> <li><a href="#functors">Functors</a></li> </ul> </div> </div>

## GHCI commands

```haskell
ghci> :?                       -- help
ghci> :q                       -- quit
ghci> :cd <directory>          -- change directory
ghci> :t <expression>          -- show type of expression
ghci> :i <type/class/function> -- show info about x
ghci> :l <file.hs>             -- load a file
ghci> :r                       -- reload current file
ghci> import <lib_name>        -- import module
ghci> :m + <lib_name>          -- add module to scope
ghci> :m - <lib_name>          -- remove module from scope
ghci> :! <command>             -- run shell command
ghci> :set <option>            -- change GHCi settings
ghci> :set prompt "ghci> "     -- change GHCi prompt
ghci> :doc <function>          -- get documentation for function
ghci> :show <info>             -- show session info (e.g., modules, bindings, imports)
```

## Indentation

Haskell relies on indentation to structure code, especially in
defining functions and grouping `code blocks`. Proper alignment
is crucial, as incorrect indentation can lead to syntax errors.

```haskell
doubleAndAdd x y =
    let doubledX = x * 2   -- 'let' introduces local variables
        doubledY = y * 2
    in doubledX + doubledY -- 'in' concludes the let expression
```

## Data Types

In Haskell, data types are essential for defining the structure and
behavior of data in a safe and expressive way. Here are some of the
fundamental data types:

- **Basic Types**:

  - `Int` - fixed-size integer numbers.`
  - `Integer` - arbitrary-precision integer numbers.
  - `Float`, `Double` - floating-point real numbers.
  - `Char` - individual characters.
  - `Bool` - boolean type, with values `True` and `False`.

- **Lists**: Lists are recursive types that contain elements of
  the same type. For example, `[Int]` is a list of integers.

- **Tuples**: Tuples allow grouping of values of different types.
  For example, `(Int, String)` is a tuple of an integer and a string.

- **Maybe**: The `Maybe` type represents a value that might be absent. It can either be `Just x` (where x is the value) or `Nothing`. This is useful for handling cases where a result might not exist.

```haskell
safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide x y = Just (x `div` y)
```

## Type Signature

A type signature specifies the types of `inputs` and `outputs` for a
function. Although optional, type signatures are recommended for
readability and debugging.

```haskell
add :: Int -> Int -> Int
add x y = x + y
```

## Function Parameters

Functions in Haskell are defined with parameters that receive arguments
directly, and they are curried by default, meaning functions
can be partially applied.

```haskell
multiply :: Int -> Int -> Int
multiply x y = x * y
```

## Let .. in

`let .. in` expressions allow you to define local variables
within a function. Variables defined in `let` are accessible only
within the corresponding `in` block.

```haskell
circleArea :: Float -> Float
circleArea r =
    let piValue = 3.14159
    in piValue * r * r

```

## If Statements

In Haskell, `if` expressions are used to make decisions based
on conditions and always require both `then` and `else` branches.

```haskell
absoluteValue :: Int -> Int
absoluteValue x = if x < 0 then -x else x
```

## Guards

`Guards` offer a cleaner syntax for defining functions with multiple
conditional branches, similar to `if` but more concise.

```haskell
factorial :: Int -> Int
factorial n
    | n == 0    = 1
    | n > 0     = n * factorial (n - 1)
    | otherwise = error "Negative input not allowed"
```

## Where Clause

The `where` clause allows defining local variables and helper functions
outside the main function body but within its scope, improving readability.

```haskell
hypotenuse :: Float -> Float -> Float
hypotenuse a b = sqrt (square a + square b)
    where
        square x = x * x
```

## Recursion

Recursion is a fundamental technique in Haskell, as it is often used in
place of traditional loops in other languages. A recursive function calls
itself with a clearly defined base case.

```haskell
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

## Pattern Matching

The `(x:xs)` notation in Haskell breaks down a
list into its head and tail. Here, `x` represents
the first element of the list, while `xs` represents the
remainder of the list.

```haskell
sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs
```

## Basic List Functions

```haskell
length x              -- Returns the length of list x
null x                -- Checks if list x is empty
head x                -- First element of list x
tail x                -- All elements of list x except the first
last x                -- Last element of list x
init x                -- All elements of list x except the last
concat [xs]           -- Concatenates a list of lists
maximum x             -- Finds the maximum value in list x
minimum x             -- Finds the minimum value in list x
sum x                 -- Sum of elements in list x
product x             -- Product of elements in list x
union list1 list2     -- Union of two lists
intersect list1 list2 -- Intersection of two lists
elem n x              -- Returns True if n is in list x, otherwise False
notElem n x           -- Returns True if n is not in list x
```

## List Transformations

```haskell
reverse x               -- Reverses the order of elements in list x
sort x                  -- Sorts list x in ascending order
nub x                   -- Removes duplicates from list x
take n x                -- Takes the first n elements of list x
drop n x                -- Drops the first n elements of list x
delete n x              -- Removes all instances of n from list x
splitAt n x             -- Splits list x into two parts at n
```

## List Operations

```haskell
x `elem` xs             -- Returns True if n is in list x, otherwise False
[a..b]                  -- Generates a list from a to b
list1 ++ list2          -- Concatenates two lists
elem : list             -- Adds element at the beginning of the list
[1, 2, 3, 4] !! 2       -- Accesses the element at index 2 (returns 3)
```

## List Comprehension

In Haskell, list comprehension provides a way to construct lists by specifying their elements based on existing lists and conditions. It's similar to set notation in mathematics.

```haskell
[ expression | generator , condition ]
```

- `expression` represents how each element in the output list is derived.
- `generator` specifies the source of elements (e.g., `x <- list`).
- `condition` is an optional filter that restricts which elements are included in the output list.

```haskell
squares = [ x^2 | x <- [1..5] ]              -- [1, 4, 9, 16, 25]
evenSquares = [ x^2 | x <- [1..10], even x ] -- [4, 16, 36, 64, 100]
```

## Zip

The `zip` function in Haskell combines two lists element-wise into a list of pairs. The result's length is the same as the shorter of the two input lists. `zip` is particularly useful for pairing up elements from related lists.

```haskell
zip [1, 2, 3] ["one", "two", "three"] -- Output: [(1, "one"), (2, "two"), (3, "three")]
```

## Sections

Sections in Haskell allow partial application of infix operators by supplying only one of the operands. When operators are enclosed in parentheses with one argument, they become functions that can be used in higher-order functions.

```haskell
addFive = (5 +)     -- Equivalent to \x -> 5 + x
divideByTwo = (/ 2) -- Equivalent to \x -> x / 2
```

## Lambda Expresions

Lambda expressions in Haskell are anonymous functions defined using the `\` symbol, often used when a function is needed only temporarily. They are useful in functions like `map` or `filter`, where defining a named function may be unnecessary.

```haskell
\param1 param2 -> expression
```

```haskell
map (\x -> x * 2) [1, 2, 3]  -- Output: [2, 4, 6]
```

## `$` Operator

The `$` operator in Haskell is a function application operator that allows you to avoid parentheses. It has low precedence, so expressions to its right are evaluated first. It simplifies code by reducing the need for nested parentheses.

```haskell
sqrt $ 3 + 4 + 5             -- Equivalent to sqrt (3 + 4 + 5)
map ($ 3) [(+2), (*4), (^2)] -- Applies each function to 3, output: [5, 12, 9]
```

## Map

The `map` function in Haskell applies a given function to each element of a list, returning a new list with the results. It’s a key tool for transforming lists without explicit loops.

```haskell
map (*2) [1, 2, 3, 4] -- Output: [2, 4, 6, 8]
```

## Filter

The `filter` function in Haskell takes a predicate and a list, returning a new list containing only the elements that satisfy the predicate. It’s often used for selecting elements from a list based on specific conditions.

```haskell
filter even [1, 2, 3, 4, 5, 6] -- Output: [2, 4, 6]
```

## Fold

In Haskell, `foldr` and `foldl` are functions that reduce a list to a single value by applying a binary operation between elements of the list and an initial "unit" value. They are fundamental for many list-based computations, like summing numbers or concatenating strings.

### foldr

`foldr` processes the list from right to left. It applies the operator to the last element and the unit value, then combines that result with the previous element, continuing until it reaches the start of the list.

```haskell
foldr op unit [a1, a2, a3, ..., an]
a1 `op` (a2 `op` (a3 `op` ... (an `op` unit)))
```

```haskell
foldr (+) 0 [1, 2, 3, 4, 5]              -- Output: 15
foldr (*) 1 [2, 3, 4]                    -- Output: 24
foldr (++) "last" ["abc", "def", "ghi"]  -- Output: "abcdefghilast"
```

### foldl

`foldl` processes the list from left to right, starting with the unit value and applying the operator to it and the first element, then to that result and the next element, and so on until the end of the list.

```haskell
foldl op unit [a1, a2, a3, ..., an]
((((unit `op` a1) `op` a2) `op` a3) ... ) `op` an
```

```haskell
foldl (+) 0 [1, 2, 3, 4, 5]               -- Output: 15
foldl (++) "first" ["abc", "def", "ghi"]  -- Output: "firstabcdefghi"
```

## Function Composition

In Haskell, the `.` operator is used for **function composition**, allowing the combination of two or more functions into a new, more concise function. Function composition is a fundamental technique in functional programming, enabling the combination of functions to create complex behaviors in an elegant and efficient way.

The `.` operator applies a function to the result of another function, having the following meaning:

```haskell
(f . g) x = f (g x)
```

## Custom Data Types

Haskell offers a powerful way to define `custom` data types using the `data`, `type`, and `newtype` keywords. Let’s look at how to use these keywords to create and use custom types, and how to call them within functions.

### 1. `data` Keyword

The `data` keyword allows you to define a new type with multiple constructors, which can take different numbers and types of arguments. For example:

```haskell
data Shape = Circle Float | Rectangle Float Float
```

### Using Custom Data Types in Functions

```haskell
area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h
```

### 2. `type` Keyword

The `type` keyword creates a type synonym, which gives a new name to an existing type for better readability or convenience. Type synonyms don’t create new types but allow you to use an alternative name:

```haskell
type Name = String
type Age = Int
type Person = (Name, Age)
```

### 3. `newtype` Keyword

The `newtype` keyword is similar to `data`, but it creates a wrapper around an existing type with only one constructor and one field. `newtype` is more efficient than `data` because it has no runtime overhead—it’s simply a label around an existing type. For example:

```haskell
newtype Email = Email String
```

This creates a new type `Email` that is distinct from `String`, even though it wraps a `String`. You can define functions that accept only `Email` rather than any `String`, which helps with type safety.

```haskell
sendEmail :: Email -> String
sendEmail (Email address) = "Sending email to " ++ address
```

To create an Email and use it in sendEmail:

```haskell
let myEmail = Email "example@example.com"
print (sendEmail myEmail)
```

## Abstract Data Types

Abstract Data Types (ADTs) in Haskell represent **data models** where implementation details are hidden, exposing only their interface. ADTs encapsulate data with operations, providing abstraction and promoting modular code.

Haskell implements ADTs primarily using **algebraic data types**, which include **sum types**, **product types**, and combinations thereof.

---

### Product Types

Product types represent data with `multiple components` that belong together. A common example is tuples or custom data types with multiple fields.

```haskell
data Point = Point Int Int -- A Point with x and y coordinates

origin :: Point
origin = Point 0 0

translate :: Point -> Int -> Int -> Point
translate (Point x y) dx dy = Point (x + dx) (y + dy)
```

### Sum Types

Sum types represent values that can be `one of several` options. These options are often implemented using constructors. `Sum types` are particularly useful for encoding states or choices.

```haskell
data Color = Red | Green | Blue

isPrimary :: Color -> Bool
isPrimary Red = True
isPrimary Green = True
isPrimary Blue = True
```

---

### Combining Product and Sum Types

`Complex ADTs` can be constructed by combining product and sum types.

```haskell
data Shape = Circle Float -- Circle with radius
| Rectangle Float Float -- Rectangle with width and height
| Triangle Float Float Float -- Triangle with three sides

perimeter :: Shape -> Float
perimeter (Circle r) = 2 _ pi _ r
perimeter (Rectangle w h) = 2 \* (w + h)
perimeter (Triangle a b c) = a + b + c
```

---

### Recursive Data Types

Recursive data types are ADTs that reference themselves. They are ideal for modeling structures like lists, trees, or graphs.

### Example: Binary Tree

```haskell
data Tree a = Leaf
| Node a (Tree a) (Tree a)

-- Function to calculate depth of a Tree
treeDepth :: Tree a -> Int
treeDepth Leaf = 0
treeDepth (Node \_ left right) =
1 + max (treeDepth left) (treeDepth right)
```

---

### Encapsulation with ADTs

Encapsulation means controlling how data and operations are exposed or hidden to protect invariants and simplify usage. Here's a simple example to make encapsulation using Abstract Data Types (ADTs) easier to understand.

### Example:

Let’s define a **Counter** data type that:

1. Stores an internal integer value.
2. Exposes only specific operations to interact with the counter.
3. Hides the actual implementation of the data.

---

### Step 1: Define the Module and Data Type

The internal details of the `Counter` are hidden by **not exporting its constructor**. Instead, we expose only the functions to work with it.

```haskell
module Counter (Counter, newCounter, increment, getValue) where

-- Internal representation of the Counter
data Counter = Counter Int

-- Function to create a new Counter, starts at 0
newCounter :: Counter
newCounter = Counter 0

-- Function to increment the Counter by 1
increment :: Counter -> Counter
increment (Counter n) = Counter (n + 1)

-- Function to retrieve the current value of the Counter
getValue :: Counter -> Int
getValue (Counter n) = n
```

---

### Step 2: Usage

Because the `Counter` constructor is not exported, users of this module can’t manipulate its internal state directly, enforcing encapsulation.

```haskell
import Counter

main :: IO ()
main = do
    -- Create a new Counter
    let c1 = newCounter

    -- Increment the Counter twice
    let c2 = increment c1
    let c3 = increment c2

    -- Get and print the current value of the Counter
    print (getValue c3)  -- Output: 2
```

### Field Accessors in Custom Data Types

When defining a custom data type, Haskell provides a convenient way to declare and automatically generate **field accessor functions**. These functions allow you to retrieve specific components of a data type in a concise and readable manner.

### Example: Binary Tree with Accessors

Consider a binary tree structure, where each node contains a value and links to its left and right children. You can define this data type in two ways: one without field accessors and another with field accessors.

**1. Without Field Accessors:**

```haskell
data Tree a b = Leaf
              | Node (a, b) (Tree a b) (Tree a b)

value :: Tree a b -> Maybe (a, b)
value Leaf          = Nothing
value (Node v _ _)  = Just v
```

Here, the `value` function is explicitly defined to extract the value from a node, requiring additional boilerplate code.

**2. With Field Accessors:**

```haskell
data Tree a b = Leaf
              | Node { value :: (a, b)
                     , left  :: Tree a b
                     , right :: Tree a b
                     }
```

In this definition:

- The `{}` syntax allows you to name the fields of the data type.
- Haskell automatically generates accessor functions (`value`, `left`, and `right`) for each field.
- You can directly use these accessor functions to retrieve field values without writing separate functions.

**Example Usage:**

```haskell
example :: Tree String Int
example = Node ("key", 42) Leaf Leaf

main :: IO ()
main = do
    let nodeValue = value example   -- Accessing the 'value' field
    print nodeValue                 -- Output: Just ("key", 42)
```

Here’s how a subchapter about **Classes in Haskell** can be added to the README. It includes detailed explanations and examples:

## Classes in Haskell

Haskell uses the concept of **type classes** to define and enforce a set of operations that types must support. Type classes are similar to interfaces or protocols in other programming languages but are more flexible and declarative.

### Overview of Type Classes

A **type class** is a collection of functions that define a common interface for a set of types. If a type is an instance of a type class, it supports and implements the functions defined in the class.

### Built-in Type Classes

Haskell provides several built-in type classes:

- `Eq`: Defines equality (`==`) and inequality (`/=`).
- `Ord`: Defines ordering operations like `<`, `>`, `<=`, `>=`.
- `Show`: Allows converting a value to a string using `show`.
- `Read`: Allows parsing a value from a string using `read`.
- `Num`: Represents numeric types.
- `Functor`, `Applicative`, `Monad`: For abstract data manipulations.

---

### Creating a Custom Type Class

You can define your own type class using the `class` keyword. For example:

```haskell
class Describable a where
    describe :: a -> String
```

Here:

- `Describable` is the name of the type class.
- `a` is a type variable that can represent any type.
- `describe` is a function that must be implemented by any type that becomes an instance of `Describable`.

---

### Making a Type an Instance of a Class

To make a type an instance of a type class, use the `instance` keyword. Here's an example:

```haskell
data Animal = Dog String Int | Cat String Int

instance Describable Animal where
    describe (Dog name age) = "Dog named " ++ name ++ ", age " ++ show age
    describe (Cat name age) = "Cat named " ++ name ++ ", age " ++ show age
```

Here:

- `Animal` is a data type with two constructors: `Dog` and `Cat`.
- The `describe` function is implemented for both constructors.

---

### Using a Custom Type Class

Now, you can use the `describe` function on any value of type `Animal`:

```haskell
main :: IO ()
main = do
    let myDog = Dog "Buddy" 5
    let myCat = Cat "Whiskers" 3
    putStrLn (describe myDog) -- Output: "Dog named Buddy, age 5"
    putStrLn (describe myCat) -- Output: "Cat named Whiskers, age 3"
```

---

### Polymorphism with Type Classes

Type classes allow for polymorphic functions that work on any type belonging to a specific class. For example:

```haskell
printDescription :: Describable a => a -> IO ()
printDescription x = putStrLn (describe x)
```

The type signature `Describable a => a -> IO ()` means:

- The function works for any type `a` as long as `a` is an instance of `Describable`.

Usage:

```haskell
main :: IO ()
main = do
    let myDog = Dog "Buddy" 5
    printDescription myDog -- Output: "Dog named Buddy, age 5"
```

---

### Advanced Example: A Class Hierarchy

You can create a hierarchy of type classes, where one type class depends on another. For example:

```haskell
class Describable a => Identifiable a where
    identify :: a -> String

instance Identifiable Animal where
    identify (Dog name _) = "Dog: " ++ name
    identify (Cat name _) = "Cat: " ++ name
```

Here:

- `Identifiable` extends `Describable`. Any type that is an instance of `Identifiable` must also be an instance of `Describable`.
- `identify` is another function defined for `Animal`.

---

### Derived Instances for Built-in Classes

Haskell can automatically derive instances of some built-in type classes, such as `Eq`, `Ord`, `Show`, and `Read`.

```haskell
data Color = Red | Green | Blue deriving (Eq, Ord, Show)

main :: IO ()
main = do
    print (Red == Green)  -- Output: False
    print (show Red)      -- Output: "Red"
    print (Red < Blue)    -- Output: True
```

## Constraints

In Haskell, **constraints** are used to restrict the types of parameters based on type classes (`type classes`). They are written before `=>` in the type signature of a function.

### Example

```haskell
add :: Num a => a -> a -> a
add x y = x + y
```

Here, `Num a` is a **constraint** that specifies that `a` must be a numeric type.

### Multiple Constraints

Multiple **constraints** can be used by separating them with a comma.

```haskell
showAndAdd :: (Show a, Num a) => a -> a -> String
showAndAdd x y = "Result: " ++ show (x + y)
```

### Common Types of Constraints

1. **`Eq`**: Types that support equality operations (`==`, `/=`).
2. **`Ord`**: Types that support ordering operations (`<`, `>`, `<=`, `>=`).
3. **`Show`**: Types that can be converted to strings.
4. **`Read`**: Types that can be parsed from strings.
5. **`Num`**: Types that support mathematical operations (`+`, `-`, `*`, etc.).

### **How does `=>` work in general?**

In Haskell, the `=>` constraints are used to express dependencies between types and type classes.

#### Structure:

```haskell
class (Constraints) => ClassName where
```

#### In instances:

```haskell
instance (Constraints) => ClassInstance where
```

#### In functions:

```haskell
function :: (Constraints) => InputType -> OutputType
```

---

## Functors

In Haskell, **functors** represent types that can be mapped over. They are defined in terms of the `Functor` type class, which provides the `fmap` function.

### `Functor` Class Signature

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

Here:

- `f` is a **type constructor**.
- `fmap` applies a function `(a -> b)` over a context `f a`.

### Examples of `Functor` Instances

1. **List (`[]`)**

```haskell
instance Functor [] where
    fmap = map
```

```haskell
exampleList :: [Int]
exampleList = fmap (*2) [1, 2, 3] -- Output: [2, 4, 6]
```

2. **The `Maybe` Type**

```haskell
instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just x) = Just (f x)
```

```haskell
exampleMaybe :: Maybe Int
exampleMaybe = fmap (+1) (Just 5) -- Output: Just 6
```

### Functor Laws

Any `Functor` instance must obey two laws:

1. **Identity**

```haskell
fmap id x == x
```

2. **Composition**

```haskell
fmap (f . g) x == fmap f (fmap g x)
```
