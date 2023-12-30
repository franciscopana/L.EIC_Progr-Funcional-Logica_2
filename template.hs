import Data.List

-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 27/12/2023

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

data StackElement = I Integer | Tt | Ff
  deriving Show
type Stack = [StackElement]

stackElem2Str :: StackElement -> String
stackElem2Str (I n) = show n
stackElem2Str Tt = "True"
stackElem2Str Ff = "False"

type State = [(String, StackElement)]

createEmptyStack :: Stack
createEmptyStack = []


stack2Str :: Stack -> String
stack2Str stack = intercalate "," (map stackElem2Str stack)

createEmptyState :: State
createEmptyState = []

state2Str :: State -> String
state2Str state = intercalate "," (map (\(var, val) -> var ++ "=" ++ stackElem2Str val) sortState)
  where sortState = sortOn fst state

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)

-- Push n operation
run ((Push n):code, stack, state) = run(code, (I n):stack, state)

-- Add operation
run (Add:code, (I n1):(I n2):stack, state) = run(code, (I (n1 + n2)):stack, state)
run (Add:code, stack, state) = error "Run-time error"

-- Mult operation
run (Mult:code, (I n1):(I n2):stack, state) = run(code, (I (n1 * n2)):stack, state)
run (Mult:code, stack, state) = error "Run-time error"

-- Sub operation
run (Sub:code, (I n1):(I n2):stack, state) = run(code, (I (n1 - n2)):stack, state)
run (Sub:code, stack, state) = error "Run-time error"

-- Push Boolean value
run (Tru:code, stack, state) = run(code, Tt:stack, state)
run (Fals:code, stack, state) = run(code, Ff:stack, state)

-- Equality operation
run (Equ:code, (I n1):(I n2):stack, state) = run(code, if n1 == n2 then Tt:stack else Ff:stack, state)
run (Equ:code, Tt:Tt:stack, state) = run(code, Tt:stack, state)
run (Equ:code, Tt:Ff:stack, state) = run(code, Ff:stack, state)
run (Equ:code, Ff:Tt:stack, state) = run(code, Ff:stack, state)
run (Equ:code, Ff:Ff:stack, state) = run(code, Tt:stack, state)
run (Equ:code, stack, state) = error "Run-time error"

-- Less than or equal operation
run (Le:code, (I n1):(I n2):stack, state) = run(code, if n1 <= n2 then Tt:stack else Ff:stack, state)
run (Le:code, Tt:Tt:stack, state) = error "Run-time error"

-- And operation
run (And:code, Tt:Tt:stack, state) = run(code, Tt:stack, state)
run (And:code, Tt:Ff:stack, state) = run(code, Ff:stack, state)
run (And:code, Ff:Tt:stack, state) = run(code, Ff:stack, state)
run (And:code, Ff:Ff:stack, state) = run(code, Ff:stack, state)
run (And:code, stack, state) = error "Run-time error"

-- Negation operation
run (Neg:code, Tt:stack, state) = run(code, Ff:stack, state)
run (Neg:code, Ff:stack, state) = run(code, Tt:stack, state)
run (Neg:code, stack, state) = error "Run-time error"

-- Fetch operation
run (Fetch var:code, stack, state) = case lookup var state of
  Just val -> run(code, val:stack, state)
  Nothing -> error "Run-time error"
  
-- Store operation
run (Store var:code, val:stack, state) | any (\(var', _) -> var == var') state = run(code, stack, (var, val):filter (\(var', _) -> var /= var') state)
                                       | otherwise = run(code, stack, (var, val):state)

-- Branch and Loop operations
run (Branch c1 c2:code, Tt:stack, state) = run(c1 ++ code, stack, state)
run (Branch c1 c2:code, Ff:stack, state) = run(c2 ++ code, stack, state)
run (Branch c1 c2:code, stack, state) = error "Run-time error"
run (Loop c1 c2:code, stack, state) = run(c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]] ++ code, stack, state)

-- Noop operation
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

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

-- data for arithmetic expressions
data Aexp = NUM Integer | VAR String | ADD Aexp Aexp | SUB Aexp Aexp | MULT Aexp Aexp
  deriving Show

-- data for boolean expressions
data Bexp = TRU | FALS | EQU Aexp Aexp | LE Aexp Aexp | AND Bexp Bexp | NEG Bexp
  deriving Show

-- data for statements
data Stm = ASSIGN String Aexp | IF Bexp Stm Stm | WHILE Bexp [Stm] | SEQ [Stm]
  deriving Show

type Program = [Stm]

-- compile arithmetic expressions
compA :: Aexp -> Code
compA (NUM n) = [Push n]
compA (VAR var) = [Fetch var]
compA (ADD a1 a2) = compA a1 ++ compA a2 ++ [Add]
compA (SUB a1 a2) = compA a1 ++ compA a2 ++ [Sub]
compA (MULT a1 a2) = compA a1 ++ compA a2 ++ [Mult]

compB :: Bexp -> Code
compB TRU = [Tru]
compB FALS = [Fals]
compB (EQU a1 a2) = compA a2 ++ compA a1 ++ [Equ]
compB (LE a1 a2) = compA a2 ++ compA a1 ++ [Le]
compB (AND b1 b2) = compB b2 ++ compB b1 ++ [And]
compB (NEG b) = compB b ++ [Neg]

compile :: Program -> Code
compile [] = []
compile (ASSIGN var a:xs) = compA a ++ [Store var] ++ compile xs
compile (IF b s1 s2:xs) = compB b ++ [Branch (compile [s1]) (compile [s2])] ++ compile xs
compile (WHILE b s:xs) = Loop (compB b) (compile s) : compile xs
compile (SEQ s:xs) = compile s ++ compile xs

-- lexer divides a string into a list of tokens
-- for example "x := 5" is transformed into ["x", ":=", "5"]
lexer :: String -> [String]
lexer [] = []
lexer (':':'=':str) = ":=":lexer str
lexer ('<':'=':str) = "<=":lexer str
lexer ('=':'=':str) = "==":lexer str
lexer (' ':str) = lexer str
lexer ('\n':str) = lexer str
lexer ('\t':str) = lexer str
lexer ('=':str) = "=":lexer str
lexer ('<':str) = "<":lexer str
lexer ('+':str) = "+":lexer str
lexer ('-':str) = "-":lexer str
lexer ('*':str) = "*":lexer str
lexer ('(':str) = "(":lexer str
lexer (')':str) = ")":lexer str
lexer (';':str) = ";":lexer str
lexer ('i':'f':str) = "if":lexer str
lexer ('t':'h':'e':'n':str) = "then":lexer str
lexer ('e':'l':'s':'e':str) = "else":lexer str
lexer ('w':'h':'i':'l':'e':str) = "while":lexer str
lexer ('d':'o':str) = "do":lexer str
lexer ('n':'o':'t':str) = "not":lexer str
lexer ('a':'n':'d':str) = "and":lexer str
lexer ('T':'r':'u':'e':str) = "True":lexer str
lexer ('F':'a':'l':'s':'e':str) = "False":lexer str
lexer (c:str) | c `elem` ['0'..'9'] = (c:takeWhile (`elem` ['0'..'9']) str):lexer (dropWhile (`elem` ['0'..'9']) str)
              | c `elem` ['a'..'z'] = (c:takeWhile (`elem` ['a'..'z']) str):lexer (dropWhile (`elem` ['a'..'z']) str)
              | otherwise = error "Lexer error"


-- Define a parser which transforms an imperative program represented as a string
-- into its corresponding representation in the Stm data (a list of statements Stm).
-- Parser must use the lexer to transform the string into a list of tokens and then
-- use the list of tokens to build the Stm data.

-- if parser receives "x := 10; x := x + 1"
-- it must call lexer and receive ["x", ":=", "10", ";", "x", ":=", "x", "+", "1"]
-- then it must transform the received list into the following list of statements:
-- [ASSIGN "x" (NUM 10), ASSIGN "x" (ADD (VAR "x") (NUM 1))]
-- so we must find ";" and between each two ";" we must find the corresponding statement.
-- Each statement is an element of the list of statements.

parse :: String -> Program
parse [] = []

-- To help you test your parser
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

