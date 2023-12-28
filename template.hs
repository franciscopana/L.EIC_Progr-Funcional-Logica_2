import Data.List
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
run ((Push n):code, stack, state) = run(code, (I n):stack, state)
run (Add:code, (I n1):(I n2):stack, state) = run(code, (I (n1 + n2)):stack, state)
run (Mult:code, (I n1):(I n2):stack, state) = run(code, (I (n1 * n2)):stack, state)
run (Sub:code, (I n1):(I n2):stack, state) = run(code, (I (n1 - n2)):stack, state)
run (Tru:code, stack, state) = run(code, Tt:stack, state)
run (Fals:code, stack, state) = run(code, Ff:stack, state)
run (Equ:code, (I n1):(I n2):stack, state) = run(code, if n1 == n2 then Tt:stack else Ff:stack, state)
run (Equ:code, Tt:Tt:stack, state) = run(code, Tt:stack, state)
run (Equ:code, Tt:Ff:stack, state) = run(code, Ff:stack, state)
run (Equ:code, Ff:Tt:stack, state) = run(code, Ff:stack, state)
run (Equ:code, Ff:Ff:stack, state) = run(code, Tt:stack, state)
run (Le:code, (I n1):(I n2):stack, state) = run(code, if n1 <= n2 then Tt:stack else Ff:stack, state)
run (And:code, Tt:Tt:stack, state) = run(code, Tt:stack, state)
run (And:code, Tt:Ff:stack, state) = run(code, Ff:stack, state)
run (And:code, Ff:Tt:stack, state) = run(code, Ff:stack, state)
run (And:code, Ff:Ff:stack, state) = run(code, Ff:stack, state)
run (Neg:code, Tt:stack, state) = run(code, Ff:stack, state)
run (Neg:code, Ff:stack, state) = run(code, Tt:stack, state)
run (Fetch var:code, stack, state) = run(code, (val):stack, state)
  where Just val = lookup var state
run (Store var:code, val:stack, state) = run(code, stack, (var, val):state)
run (Noop:code, stack, state) = run(code, stack, state)
run (Branch code1 code2:code, Tt:stack, state) = run(code, stack, state)
run (Branch code1 code2:code, Ff:stack, state) = run(code, stack, state)
run (Branch code1 code2:code, stack, state) = run(code, stack, state)
run (Loop code1 code2:code, Tt:stack, state) = run(code, stack, state)
run (Loop code1 code2:code, Ff:stack, state) = run(code, stack, state)
run (Loop code1 code2:code, stack, state) = run(code, stack, state)

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
-- MAL testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

-- compA :: Aexp -> Code
compA = undefined -- TODO

-- compB :: Bexp -> Code
compB = undefined -- TODO

-- compile :: Program -> Code
compile = undefined -- TODO

-- parse :: String -> Program
parse = undefined -- TODO

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

