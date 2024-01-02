import Data.List
import Data.Char (isLower)

import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (emptyDef)

-- PFL 2023/24 - Haskell practical assignment quickstart

-- Part 1

-- Inst is the type of instructions
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving (Eq, Show)
type Code = [Inst]

-- The stack may contain integer or boolean values (Tt - True and Ff - False)
data StackElement = I Integer | Tt | Ff
  deriving (Eq, Show)
type Stack = [StackElement]

-- Convert a stack element to a string
stackElem2Str :: StackElement -> String
stackElem2Str (I n) = show n
stackElem2Str Tt = "True"
stackElem2Str Ff = "False"

-- The state is a list of pairs (variable name, value).
-- The value is a stack element.
type State = [(String, StackElement)]

-- Create an empty stack (it is just an empty list)
createEmptyStack :: Stack
createEmptyStack = []

-- Convert a stack to a string (comma separated list of stack elements)
stack2Str :: Stack -> String
stack2Str stack = intercalate "," (map stackElem2Str stack)

-- Create an empty state (it is just an empty list)
createEmptyState :: State
createEmptyState = []

-- Convert a state to a string (comma separated list of variable=value pairs)
state2Str :: State -> String
state2Str state = intercalate "," (map (\(var, val) -> var ++ "=" ++ stackElem2Str val) sortState)
  where sortState = sortOn fst state

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)

-- Push n operation (insert an integer on the stack)
run ((Push n):code, stack, state) = run(code, (I n):stack, state)

-- Add operation (add the two topmost integers on the stack, pop them and push the result (integer))
run (Add:code, (I n1):(I n2):stack, state) = run(code, (I (n1 + n2)):stack, state)
run (Add:code, stack, state) = error "Run-time error"

-- Mult operation (multiply the two topmost integers on the stack, pop them and push the result (integer))
run (Mult:code, (I n1):(I n2):stack, state) = run(code, (I (n1 * n2)):stack, state)
run (Mult:code, stack, state) = error "Run-time error"

-- Sub operation (subtract the two topmost integers on the stack, pop them and push the result (integer))
run (Sub:code, (I n1):(I n2):stack, state) = run(code, (I (n1 - n2)):stack, state)
run (Sub:code, stack, state) = error "Run-time error"

-- Push Boolean value (insert Tt or Ff on the stack)
run (Tru:code, stack, state) = run(code, Tt:stack, state)
run (Fals:code, stack, state) = run(code, Ff:stack, state)

-- Equality operation (compare the two topmost elements on the stack, pop them and push the result (boolean))
run (Equ:code, s1:s2:stack, state)
  | s1 == s2 = run(code, Tt:stack, state)
  | otherwise  = run(code, Ff:stack, state)
run (Equ:code, stack, state) = error "Run-time error"

-- Less than or equal operation (compare the two topmost elements on the stack, pop them and push the result (boolean))
run (Le:code, (I n1):(I n2):stack, state) = run(code, if n1 <= n2 then Tt:stack else Ff:stack, state)
run (Le:code, Tt:Tt:stack, state) = error "Run-time error"

-- And operation (compare the two topmost elements on the stack, pop them and push the result (boolean))
-- only boolean values are allowed to be compared
run (And:code, Tt:Tt:stack, state) = run(code, Tt:stack, state)
run (And:code, Tt:Ff:stack, state) = run(code, Ff:stack, state)
run (And:code, Ff:Tt:stack, state) = run(code, Ff:stack, state)
run (And:code, Ff:Ff:stack, state) = run(code, Ff:stack, state)
run (And:code, stack, state) = error "Run-time error"

-- Negation operation (compare the topmost element on the stack, pop it and push the result (boolean))
-- only boolean values are allowed to be compared
run (Neg:code, Tt:stack, state) = run(code, Ff:stack, state)
run (Neg:code, Ff:stack, state) = run(code, Tt:stack, state)
run (Neg:code, stack, state) = error "Run-time error"

-- Fetch operation (get the value of a variable from the state and push it on the stack)
run (Fetch var:code, stack, state) = case lookup var state of
  Just val -> run(code, val:stack, state)
  Nothing -> error "Run-time error"
  
-- Store operation (pop the topmost element from the stack and store it in the state)
-- if the variable is already in the state, replace its value
-- otherwise, add a new variable=value pair to the state
run (Store var:code, val:stack, state) | any (\(var', _) -> var == var') state = run(code, stack, (var, val):filter (\(var', _) -> var /= var') state)
                                       | otherwise = run(code, stack, (var, val):state)

-- Branch and Loop operations
-- Branch c1 c2:code - if the topmost element on the stack is Tt, execute c1, otherwise execute c2
run (Branch c1 c2:code, Tt:stack, state) = run(c1 ++ code, stack, state)
run (Branch c1 c2:code, Ff:stack, state) = run(c2 ++ code, stack, state)
run (Branch c1 c2:code, stack, state) = error "Run-time error"

-- Loop c1 c2:code - execute c1, then if the topmost element on the stack is Tt, execute c2 and then Loop c1 c2
run (Loop c1 c2:code, stack, state) = run(c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]] ++ code, stack, state)

-- Noop operation (do nothing)
run (Noop:code, stack, state) = run(code, stack, state)

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
-- IF you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- IF you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"

-- run all testAssembler tests
runTests1 :: IO ()
runTests1 = mapM_ runTest testCases
  where
    runTest (input, expected) = do
      let result = testAssembler input
      if result == expected
        then putStrLn $ "Passed: " ++ show input
        else putStrLn $ "Failed: " ++ show input ++ " expected " ++ show expected ++ " but got " ++ show result
    testCases = [
        ([Push 10,Push 4,Push 3,Sub,Mult], ("-10","")),
        ([Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"], ("","a=3,someVar=False,var=True")),
        ([Fals,Store "var",Fetch "var"], ("False","var=False")),
        ([Push (-20),Tru,Fals], ("False,True,-20","")),
        ([Push (-20),Tru,Tru,Neg], ("False,True,-20","")),
        ([Push (-20),Tru,Tru,Neg,Equ], ("False,-20","")),
        ([Push (-20),Push (-21), Le], ("True","")),
        ([Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"], ("","x=4")),
        ([Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]], ("","fact=3628800,i=1"))
      ]


-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

-- Arithmetic basic expressions
-- NUM x - x is an integer number,
-- VAR "x" - x is a variable name,
-- ADD a1 a2 - a1 and a2 are arithmetic expressions, represents the sum of a1 and a2,
-- SUB a1 a2 - a1 and a2 are arithmetic expressions, represents the difference of a1 and a2,
-- MULT a1 a2 - a1 and a2 are arithmetic expressions, represents the product of a1 and a2,
data Aexp = NUM Integer | VAR String | ADD Aexp Aexp | SUB Aexp Aexp | MULT Aexp Aexp
  deriving Show

-- Boolean basic expressions
-- TRU - represents the boolean value True,
-- FALS - represents the boolean value False,
-- EQU a1 a2 - a1 and a2 are arithmetic expressions, represents the equality of a1 and a2,
-- EQUB b1 b2 - b1 and b2 are boolean expressions, represents the equality of b1 and b2,
-- LE a1 a2 - a1 and a2 are arithmetic expressions, represents the less than or equal comparison of a1 and a2,
data Bexp = TRU | FALS | EQU Aexp Aexp | EQUB Bexp Bexp | LE Aexp Aexp | AND Bexp Bexp | NEG Bexp
  deriving Show

-- Statements
-- ASSIGN "x" a - x is a variable name, a is an arithmetic expression, represents the assignment of a to x,
-- IF b s1 s2 - b is a boolean expression, s1 and s2 are statements, represents the if-then-else statement,
-- WHILE b [s] - b is a boolean expression, s is a statement, represents the while statement,
data Stm = ASSIGN String Aexp | IF Bexp Stm Stm | WHILE Bexp [Stm] | SEQ [Stm]
  deriving Show

-- a Program is a list of statements
type Program = [Stm]

-- compile arithmetic expressions
{-|
  The 'compA' function compiles an arithmetic expression into a sequence of instructions.
  It takes an 'Aexp' (arithmetic expression) as input and returns 'Code' (a list of 'Inst' values).
  The function handles the following cases:
  * 'NUM n': Pushes the number 'n' onto the stack.
  * 'VAR var': Fetches the value of the variable 'var' from the state and pushes it onto the stack.
  * 'ADD a1 a2': Compiles 'a2', then 'a1', and then performs an addition operation.
  * 'SUB a1 a2': Compiles 'a2', then 'a1', and then performs a subtraction operation.
  * 'MULT a1 a2': Compiles 'a2', then 'a1', and then performs a multiplication operation.
-}
compA :: Aexp -> Code
compA (NUM n) = [Push n]
compA (VAR var) = [Fetch var]
compA (ADD a1 a2) = compA a2 ++ compA a1 ++ [Add]
compA (SUB a1 a2) = compA a2 ++ compA a1 ++ [Sub]
compA (MULT a1 a2) = compA a2 ++ compA a1 ++ [Mult]

-- compile boolean expressions
{-|
  The 'compB' function compiles a boolean expression into a sequence of instructions.
  It takes a 'Bexp' (boolean expression) as input and returns 'Code' (a list of 'Inst' values).
  The function handles the following cases:
  * 'TRU': Pushes a boolean `True` onto the stack.
  * 'FALS': Pushes a boolean `False` onto the stack.
  * 'EQU a1 a2': Compiles 'a2', then 'a1', and then checks if they are equal.
  * 'EQUB b1 b2': Compiles 'b2', then 'b1', and then checks if they are equal.
  * 'LE a1 a2': Compiles 'a2', then 'a1', and then checks if the first is less than or equal to the second.
  * 'AND b1 b2': Compiles 'b2', then 'b1', and then performs a logical AND operation.
  * 'NEG b': Compiles 'b' and then negates it.
-}
compB :: Bexp -> Code
compB TRU = [Tru]
compB FALS = [Fals]
compB (EQU a1 a2) = compA a2 ++ compA a1 ++ [Equ]
compB (EQUB b1 b2) = compB b2 ++ compB b1 ++ [Equ]
compB (LE a1 a2) = compA a2 ++ compA a1 ++ [Le]
compB (AND b1 b2) = compB b2 ++ compB b1 ++ [And]
compB (NEG b) = compB b ++ [Neg]

-- compile a list of statements
{-|
  The 'compile' function compiles a list of statements into a sequence of instructions.
  It takes a 'Program' (a list of 'Stm' values) as input and returns 'Code' (a list of 'Inst' values).
  The function handles the following cases:
  * 'ASSIGN var a': Compiles the arithmetic expression 'a', stores the result in the variable 'var', and then compiles the rest of the program.
  * 'IF b s1 s2': Compiles the boolean expression 'b', branches to either the compiled 's1' or 's2' depending on the result, and then compiles the rest of the program.
  * 'WHILE b s': Creates a loop that repeatedly executes the compiled 's' while the boolean expression 'b' is true, and then compiles the rest of the program.
  * 'SEQ s': Compiles the list of statements 's', and then compiles the rest of the program.
-}
compile :: Program -> Code
compile [] = []
compile (ASSIGN var a:xs) = compA a ++ [Store var] ++ compile xs
compile (IF b s1 s2:xs) = compB b ++ [Branch (compile [s1]) (compile [s2])] ++ compile xs
compile (WHILE b s:xs) = Loop (compB b) (compile s) : compile xs
compile (SEQ s:xs) = compile s ++ compile xs

-- Parsers

-- 'lexer' is a function that creates a lexer for a simple language.
-- The lexer is created using the 'makeTokenParser' function from the Text.Parsec.Token module,
-- and it is configured with a language definition that is mostly empty ('emptyDef'), 
-- but with a few specific settings for reserved operator names and reserved names.
--
-- The reserved operator names are the following:
-- "+", "-", "*", ":=", ";", "<=", "=="
--
-- The reserved names are the following:
-- "True", "False", "if", "then", "else", "while", "do", "and", "not"

keywords :: [String]
keywords = ["True", "False", "if", "then", "else", "while", "do", "and", "not"]

lexer :: T.TokenParser ()
lexer = T.makeTokenParser $ emptyDef
  {
      T.reservedOpNames = ["+", "-", "*", ":=", ";", "<=", "=="],
      T.reservedNames = keywords
  }



-- 'identifier' is a parser that parses an identifier (variable name).
-- An identifier is a string that starts with a lowercase letter and can contain lowercase letters and digits.
-- The parser fails if the identifier is a reserved name or operator.


identifier :: Parser String
identifier = do
  ident <- T.identifier lexer
  if any (`isInfixOf` ident) keywords || not (isLower (case ident of (x:_) -> x))
    then fail $ "invalid variable name: " ++ ident
    else return ident



-- 'number' is a parser that parses an integer number.
-- The parser fails if the number is not an integer.
number :: Parser Aexp
number = NUM . read <$> P.many1 P.digit


-- Parses a variable expression.
variable :: Parser Aexp
variable = VAR <$> identifier


-- Parses a factor expression.
-- A factor can be a variable, a number, or an expression enclosed in parentheses.
factor :: Parser Aexp
factor = P.try variable 
  P.<|> number 
  P.<|> (P.char '(' *> expr <* P.char ')')

-- The 'multiplication' parser parses a multiplication operation in an arithmetic expression.
-- It expects a '*' character surrounded by spaces, followed by a factor expression.
-- It returns a tuple containing a function that represents the multiplication operation and the parsed factor expression.
multiplication :: Parser (Aexp -> Aexp, Aexp)
multiplication = do
  P.spaces
  _ <- P.char '*'
  P.spaces
  e2 <- factor
  return ((\e1 -> MULT e1 e2), e2)

-- Parses an addition expression.
-- The parser expects a '+' character followed by a term.
-- It returns a tuple containing a function that adds the parsed term to an arithmetic expression,
-- and the parsed term itself.
addition :: Parser (Aexp -> Aexp, Aexp)
addition = do
  P.spaces
  _ <- P.char '+'
  P.spaces
  e2 <- term
  P.spaces
  return ((\e1 -> ADD e1 e2), e2)

--   Parses a subtraction expression and returns a tuple containing a function
--   that subtracts the parsed expression from another arithmetic expression,
--   and the parsed expression itself.
subtraction :: Parser (Aexp -> Aexp, Aexp)
subtraction = do
  P.spaces
  _ <- P.char '-'
  P.spaces
  e2 <- term
  P.spaces
  return ((\e1 -> SUB e1 e2), e2)

-- 'term' is a parser that parses a term in an arithmetic expression.
-- A term is defined as a factor followed by zero or more multiplication operations.
-- Each multiplication operation is represented by a tuple containing a function that multiplies its input by a factor, and the factor itself.
-- The parser returns an arithmetic expression that represents the term.

term :: Parser Aexp
term = do
  f <- factor
  rest <- P.many (P.try multiplication)
  return $ foldl (\acc (op, val) -> op acc) f rest

-- 'expr' is a parser that parses an arithmetic expression.
-- An expression is defined as a term followed by zero or more addition or subtraction operations.
-- Each operation is represented by a tuple containing a function that adds or subtracts its input by a term, and the term itself.
-- The parser returns an arithmetic expression that represents the expression.

expr :: Parser Aexp
expr = do
  t <- term
  rest <- P.many (P.try addition P.<|> subtraction)
  return $ foldl (\acc (op, val) -> op acc) t rest

-- Parses an assignment statement.
-- The assignment statement consists of a variable, followed by ":=",
-- followed by an expression, and ends with a semicolon.
-- Returns a 'Stm' representing the assignment.

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


-- Parses an equality arithmetic expression.
-- The expression can be a combination of expressions, terms, and factors.
-- It expects the format: a1 == a2
-- Returns a 'Bexp' representing the equality expression.
equalityAexp :: Parser Bexp
equalityAexp = do
  a1 <- P.try expr P.<|> P.try term P.<|> factor
  P.spaces
  _ <- P.string "=="
  P.spaces
  a2 <- P.try expr P.<|> P.try term P.<|> factor
  return $ EQU a1 a2

-- Parses an equality boolean expression.
-- The expression should be in the form: b1 = b2
-- Returns a 'Bexp' representing the equality.
equalityBexp :: Parser Bexp
equalityBexp = do
  b1 <- simpleBooleanWithoutEqualityBexp
  P.spaces
  _ <- P.string "="
  P.spaces
  b2 <- simpleBooleanWithoutEqualityBexp
  return $ EQUB b1 b2

-- Parser for simple boolean expressions without equality.
-- This parser handles the following cases:
--   - Parsing the string "True" and returning the TRU constructor
--   - Parsing the string "False" and returning the FALS constructor
--   - Parsing an inequality expression
--   - Parsing an equality arithmetic expression
--   - Parsing a negation expression
--   - Parsing a boolean expression enclosed in parentheses
simpleBooleanWithoutEqualityBexp :: Parser Bexp
simpleBooleanWithoutEqualityBexp = 
  P.try (P.string "True" >> return TRU)
  P.<|> P.try (P.string "False" >> return FALS)
  P.<|> P.try inequality
  P.<|> P.try equalityAexp
  P.<|> P.try negation
  P.<|> P.try (P.char '(' *> boolean <* P.char ')')


--  Parses an inequality expression of the form `a1 <= a2`.
--  Returns a `Bexp` representing the parsed inequality.
inequality :: Parser Bexp
inequality = do
  a1 <- factor
  P.spaces
  _ <- P.string "<="
  P.spaces
  a2 <- factor
  return $ LE a1 a2

-- Parser for simple boolean expressions.
-- This parser tries to parse a simple boolean expression without equality,
-- and if that fails, it tries to parse an equality boolean expression.
simpleBoolean :: Parser Bexp
simpleBoolean = 
  P.try simpleBooleanWithoutEqualityBexp
  P.<|> P.try equalityBexp

-- 'negation' is a parser that parses a negation operation in a boolean expression.
-- A negation operation is defined as the string "not" followed by a simple boolean expression.
-- The parser returns a boolean expression that represents the negation of the parsed boolean expression.
negation :: Parser Bexp
negation = do
  P.spaces
  _ <- P.string "not"
  P.spaces
  bexp <- simpleBoolean
  return $ NEG bexp

-- Parses a boolean term.
--
-- This function uses the `equalityBexp` parser or the `simpleBoolean` parser to parse the first boolean expression.
-- Then, it uses the `conjunction` parser to parse any additional boolean expressions, and folds them using the given
-- operator. The result is a parsed boolean expression.
boolTerm :: Parser Bexp
boolTerm = do
  f <- P.try equalityBexp P.<|> simpleBoolean
  fs <- P.many (P.try conjunction)
  return $ foldl (\acc op -> op acc) f fs

-- | Parses a conjunction expression and returns a function that combines it with another boolean expression using the 'AND' operator.
conjunction :: Parser (Bexp -> Bexp)
conjunction = do
  P.spaces
  _ <- P.string "and"
  P.spaces
  bexp <- boolTerm
  return (`AND` bexp)

-- | Parses a boolean expression.
boolean :: Parser Bexp
boolean = boolTerm

-- | Parses an if statement and returns a 'Stm' representing the parsed statement.
--
-- The if statement has the following structure:
--   if <boolean expression> then <statement> else <statement>
--
-- The boolean expression is parsed using the 'boolean' parser.
-- The 'statement' parser is used to parse the statements in the 'then' and 'else' branches.
--
-- If the statements in the 'then' or 'else' branches are enclosed in parentheses, they are parsed as a sequence of statements.
-- Otherwise, a single statement is parsed.
--
-- The parsed if statement is represented using the 'IF' constructor of the 'Stm' type.

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



-- Parses a while statement.
-- The while statement consists of the keyword "while", followed by a boolean expression,
-- the keyword "do", and a sequence of statements enclosed in parentheses or a single statement.
-- Returns a 'Stm' representing the while statement.
whileStatement :: Parser Stm
whileStatement = do
  P.spaces
  _ <- P.string "while"
  P.spaces
  bexp <- boolean
  P.spaces
  _ <- P.string "do"
  P.spaces
  stm <- P.try (P.char '(' *> (toSeq <$> P.many1 statement) <* P.char ')') P.<|> statement
  return $ WHILE bexp [stm]
  where
    toSeq [x] = x
    toSeq xs = SEQ xs


-- Parses a statement.
--
-- This function parses a statement using the 'assignment', 'ifStatement', or 'whileStatement' parsers.
-- It skips any leading spaces before parsing the statement and ensures there are spaces after the statement.
--
-- Returns the parsed statement.
statement :: Parser Stm
statement = do
  P.spaces
  stmt <- P.try assignment P.<|> ifStatement P.<|> whileStatement
  P.spaces
  return stmt

-- Parses a list of statements.
-- Returns a parser that consumes zero or more whitespace characters followed by a statement.
statements :: Parser [Stm]
statements = P.many (P.spaces >> statement)

-- Parses a string into a program.
-- If the parsing fails, it throws an error with the parse error message.
parse :: String -> Program
parse str = case P.parse statements "" str of
  Left err -> error $ show err
  Right program -> program


-- This function takes a string representing program code and returns a tuple containing the string representation of the stack and the string representation of the state after running the program.
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "x := 0 - 2;" == ("","x=-2")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
-- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
-- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
-- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")

runTests2 :: IO ()
runTests2 = mapM_ runTest testCases
  where
    runTest (input, expected) = do
      let result = testParser input
      if result == expected
        then putStrLn $ "Passed: " ++ input
        else putStrLn $ "Failed: " ++ input ++ " expected " ++ show expected ++ " but got " ++ show result
    testCases = [
        ("x := 5; x := x - 1;", ("","x=4")),
        ("x := 0 - 2;", ("","x=-2")),
        ("if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;", ("","y=2")),
        ("x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);", ("","x=1")),
        ("x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;", ("","x=2")),
        ("x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;", ("","x=2,z=4")),
        ("x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;", ("","x=34,y=68")),
        ("x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;", ("","x=34")),
        ("if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;", ("","x=1")),
        ("if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;", ("","x=2")),
        ("x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);", ("","x=2,y=-10,z=6")),
        ("i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);", ("","fact=3628800,i=1"))
      ]