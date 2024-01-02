# PFL: Coursework-Haskel

> Group T08_G10:
Francisco da Ana (up202108762): 50%
José Pedro Evans (up202108818): 50%

## Part 1

This Haskell code defines a simple stack-based virtual machine. The machine operates on instructions, stack elements, and a state.

The `Inst` data type represents the instructions that the machine can execute. These include arithmetic operations like `Push`, `Add`, `Mult`, `Sub`, boolean operations like `Tru`, `Fals`, `And`, `Neg`, and control flow operations like `Branch` and `Loop`.

The `StackElement` data type represents the elements that can be stored on the stack. These can be either integers (`I Integer`) or boolean values (`Tt` for True, `Ff` for False).

The `State` type represents the state of the machine, which is a list of pairs of variable names and their corresponding values (which are stack elements).

The `run` function is the main function that executes the instructions. It takes a tuple of code (a list of instructions), stack, and state, and returns the updated code, stack, and state after executing the instructions. The function is defined recursively, with a separate case for each type of instruction.
We've implemented the `run` function taking into consideration the expected behavior for each type of instruction. During their execution, runtime errors are detected if the operands of these instructions are invalid.

```haskell!
run (Add:code, (I n1):(I n2):stack, state) = run (code, (I (n1+n2)):stack, state)
run (Add:code, stack, state) = error "Run-time error"
```

> This is an example of the definition of a run function. It represents the add operation: add the two topmost integers on the stack, pop them and push the result (integer). If the operations is a `Add` and the operands are not 2 integers, it is thrown a runtime error

The `testAssembler` function is a helper function to test the assembler. It takes a list of instructions (code), runs them, and returns the final stack and state as strings.

Having completed the implementations related to Part 1, we are capable of receiving a list of instructions from among the basic instructions defined for the language and execute them. 
However, we are still far from being able to interpret a string corresponding to actual code. To achieve this, we need to process it with a parser that generates an _Abstract Syntax Tree_ corresponding to the indicated code. This tree will then be compiled, generating the aforementioned list of instructions that we can already execute.

## Part 2

The compilation part of the code is responsible for transforming the parsed expressions and statements - represented in an _Abstract Syntax Tree_ structure - into instructions that can be executed by the virtual machine.

The `Aexp` data type represents arithmetic expressions. These can be integers (`NUM Integer`), variables (`VAR String`), or binary operations (`ADD`, `SUB`, `MULT`) on two arithmetic expressions.
The `compileAexp` function compiles an arithmetic expression into a list of instructions. It pattern matches on the structure of the expression and generates the appropriate instructions. For example, if the expression is a `NUM`, it generates a `Push` instruction and if the expressions is a `VAR`, it generates a `Fetch` instruction. If the expression is a binary operation, it recursively compiles the operands and then generates an `Add`, `Sub`, or `Mult` instruction.

The `Bexp` data type represents boolean expressions. These can be boolean constants (`TRU`, `FALS`), comparisons of arithmetic expressions (`EQU`, `LE`), comparisons of boolean expressions (`EQUB`), negations (`NEG`), or binary operations (`AND`) on two boolean expressions.
The `compileBexp` function compiles a boolean expression into a list of instructions. It also pattern matches on the structure of the expression. For example, if the expression is a boolean constant, it generates a `Tru` or `Fals` instruction. If the expression is a comparison operation, it compiles the operands and then generates an `Equ`, `Le`, `And` or `Neg` instruction.

The `Stm` data type represents statements in the language. These can be either assignment statements (`ASSIGN String Aexp`) or control flow statements like if, whiles and sequences (`IF`, `WHILE`, `SEQ`).
The `compile` function compiles a list of statements into a list of instructions. If the statement is an assignment, it compiles the right-hand side expression and then generates a `Store` instruction.  The `IF` statement generates a `Branch` instruction, the `WHILE` statement generates a `Loop` instruction and a `SEQ` generates a sequence of 2 statements to be compiled.

The `expr`, `term`, `factor`, `boolean`, `simpleBoolean`, `statement`, and `assignment` functions are parsers for expressions, terms, factors, boolean expressions, simple boolean expressions, statements, and assignment statements, respectively. Each parser uses the Parsec library to define a context-free grammar for its corresponding language construct.

The `expr` parser, for example, parses an expression, which is defined as a term followed by zero or more addition or subtraction operations. Each operation is represented by a tuple containing a function that adds or subtracts its input by a term, and the term itself. The parser returns an arithmetic expression that represents the expression.

```haskell!
assignment :: Parser Stm
assignment = do
  var <- identifier
  P.spaces
  _ <- P.string ":="
  P.spaces
  e <- expr
  P.spaces
  _ <- P.char ';'
  return $ ASSIGN var e

```

> This is the example of the assignment parser that parses an assignment statement. It consists of a variable, followed by ":=", followed by an expression, and ends with a semicolon. Returns a 'Stm' representing the assignment.

```haskell!
ifStatement :: Parser Stm
ifStatement = do
  P.spaces
  _ <- P.string "if"
  P.spaces
  bexp <- boolean
  P.spaces
  _ <- P.string "then"
  P.spaces
  stm1 <- P.try (P.char '(' *> (toSeq <$> P.many1 statement) <* P.char ')') P.<|> statement
  P.spaces
  _ <- P.string "else"
  P.spaces
  stm2 <- P.try (P.char '(' *> (toSeq <$> P.many1 statement) <* P.char ')' <* P.char ';') P.<|> statement
  return $ IF bexp stm1 stm2
  where
    toSeq [x] = x
    toSeq xs = SEQ xs
```

> This is the example of the if statement parser.  It Parses an if statement and returns a 'Stm' representing the parsed statement, following this structure: **if <boolean expression> then <statement> else <statement>**. The boolean expression is parsed using the 'boolean' parser. The 'statement' parser is used to parse the statements in the 'then' and 'else' branches.
 If the statements in the 'then' or 'else' branches are enclosed in parentheses, they are parsed as a sequence of statements. Otherwise, a single statement is parsed.

The `boolean` parser parses a boolean expression, which is defined as a simple boolean expression followed by zero or more conjunction operations. Each operation is represented by a tuple containing a function that performs a logical AND operation on its input and a simple boolean expression, and the simple boolean expression itself. The parser returns a boolean expression that represents the boolean expression.

The `statement` parser parses a statement, which is defined as an assignment statement or a compound statement. The parser returns a statement that represents the statement.
