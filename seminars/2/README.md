# Mathematical expression interpreter

Your task is to:
1. Write monadic parsing combinators
2. Define a structure to represent a parsed mathematical expression in Haskell
3. Write a parser, which parses a string into the structure
4. Write an interpreter for the structure, which performs the calculation and returns the result.

**NOTE: the parser and interpreter should be independent.**

You will have to handle division by zero errors.

## The basic expression

### Formal Definition

Note that the expressions can be nested arbitrarily deeply.

Also note, that a number can only be an integer.

The `/` operator is integer division truncated towards negative infinity (`div` in Haskell).

```bnf
<expression> ::= <number> | <operation>

<number> ::= "-" <digits> | <digits>

<digits> ::= <digit> | <digit> <digits>
<digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

<operation> ::= "(" <spaces> <expression> <spaces> <operator> <spaces> <expression> <spaces> ")"

<operator> ::= "+" | "-" | "*" | "/"

<spaces> ::= "" | " " <spaces>
```

### Examples

These all represent valid expressions:

- `((1 + 2) * 3)`
- `1`
- `(((1 + 2) *3) +(1  +  1)   )`

### Minimal required combinators

This combinator should read and return exactly one character from the input.
If there are no characters to read from the input, then it should fail.

```haskell
anyToken :: Parser Char
```

This combinator should read and return exactly one character from the input.
If the character does not match the given character, then it should fail.

```haskell
token :: Char -> Parser ()
```

This combinator should read a string of charracters. If the string does not match the given string then fail.

```haskell
tokens :: String -> Parser ()
```

This character reads any number of consecutive characters, which stisfy the predicate. So this can never fail.

```haskell
tokensWhile :: (Char -> Bool) -> Parser String
```

This character reads any number (but at least one) of consecutive characters, which stisfy the predicate. This can only fail if the first character does not satisfy the predicate.

```haskell
tokensWhile1 :: (Char -> Bool) -> Parser String
```

### Simple example

A sample parser might look something like this

```haskell
number :: Parser Int
number = do
  sign <- signParser
  digits <- digitsParser
  return (read (sign <> digits))
```

### Control

The proposed approach of handling control is using the `Alternative` typeclass.

The instance for `[]` already provides the following behavior:

`empty` represents a failure (no values).

`<|>` represents parsing alternative. For example an expression can be either an operation or a number.

## Complex expressions

This is a slightly more complex version of expressions -- it includes conditional expressions

This represents the new parts:

```bnf
<expression> ::= <number> | <operation> | <conditional>

<conditional> ::= "if " <spaces> <bool_expression> <spaces> " then " <spaces> <expression> <spaces> " else " <spaces> <expression>

<bool_expression> ::= "(" <spaces> <expression> <spaces> <bool_operator> <spaces> <expression> <spaces> ")"

<bool_operator> = "=" | "<" | ">" | "<=" | ">="
```

### Examples

These all represent valid expressions:

- `if ( 1 = 2 ) then (2 + 4) else 2`
- `(((1 + 2) *3) +( if ( 1 = 2 ) then (2 + 4) else 2  +  1))`

## Artifacts

You should export the `calculate` function, which should parse the given string and return the computed result. If any of the steps involved fail (couldn't parse the string or there was a divide by zero error), you should return `Nothing`.

```haskell
calculate :: String -> Maybe Int
```

## Libraries you can use

You can use these two libraries:
- `base`
- `mtl`

