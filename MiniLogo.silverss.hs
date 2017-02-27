-- Steven Silvers, silverss

module MiniLogo where

import Prelude hiding (Num)

import Data.List --for pretty print

-- From assignment:
-- num  ::= (any natural number)  
-- var  ::= (any variable name) 
-- macro  ::= (any macro name)  

-- prog ::= ε   |   cmd ; prog  sequence of commands
      
-- mode ::= down   |   up pen status
      
-- expr ::= var variable reference
-- |  num literal number
-- |  expr + expr addition expression
      
--cmd ::= pen mode  change pen mode
-- |  move ( expr , expr )  move pen to a new position
-- |  define macro ( var* ) { prog }    define a macro
-- |  call macro ( expr* )  invoke a macro

-- 1.) define abstract syntax

type Num = Int
type Var = String
type Macro = String
type Prog = [Cmd]

data Mode = Up
          | Down
   deriving (Eq,Show)

data Expr = Refer Var
          | Num Int
          | Add Expr Expr
   deriving (Eq,Show)

data Cmd = Pen Mode
         | Move Expr Expr
         | Define Macro [Var] Prog
         | Call Macro [Expr]
  deriving (Eq,Show)

-- 2.) define line macro
-- lift pen up, move to starting coordinate, set pen down, move to finish coordinate

-- define line (x1,y1,x2,y2) 
--       Pen Up; Move (x1,y1); Pen Down; Move (x2,y2);

line :: Cmd
line = Define "line" ["x1", "y1", "x2", "y2"] [Pen Up, Move (Refer "x1") (Refer "y1"), Pen Down, Move (Refer "x2") (Refer "y2")]


-- 3.) use line to define nix macro
-- manipulate line calls with starting reference x,y by adding w to x or h to y

-- Define nix (x,y,w,h) {
--     line(x,y,x+w,y+h) -- bottom left corner to top right corner
--     line(x,y+h,x+w,y) --  top right corner to bottom left corner
-- }

nix :: Cmd
nix = Define "nix" ["x","y","w","h"] 
                   [Call "line" [(Refer "x"), (Refer "y"), (Add (Refer "x") (Refer "w")), (Add (Refer "y") (Refer "h"))]
                  , Call "line" [(Refer "x"), (Add (Refer "y") (Refer "h")), (Add (Refer "x") (Refer "w")), (Refer "y")] ]

-- 4.) Define Haskell function Steps that creates a MiniLogo Program
--     That draws a staircase of n steps starting from (0,0)
-- Idea: start at nth step, draw nth step then call step (n-1) until n=0

steps :: Int -> Prog
steps 0 = []
steps n = [Call "line" [Num (n-1), Num (n-1), Num (n-1), Num n], Call "line" [Num (n-1), Num n, Num n, Num n]] ++ steps (n-1)

-- 5.) define a Haskell function macros that returns a list of the names of all macros
--      that are defined anywhere in a given MiniLogo program
--   Base case: empty list returns empty list
--   check if the Cmd is a macro, recursively append macros onto list as they appear in the Prog
--   and ignore everything else
--   a macro is always followed by a var list and a prog


macros :: Prog -> [Macro]
macros [] = []
macros (x:xs) = case x of Define m v p -> m:macros xs -- matches macros pattern, add to list
                          otherwise -> macros xs --doesn't match, keep going through list

-- 6.) Define a haskell function that pretty prints a MiniLogo Program
--     Just need to make a case for each command, and a helper to deal with expressions

pretty :: Prog -> String
pretty [] = ""
pretty (Define m xs y:ys) = "Define " ++ m ++ " (" ++ concat (intersperse ", " xs) ++ ")" ++ "{ \n" ++ (pretty y) ++ "}\n"
pretty (Call m xs:ys) = "Call " ++ m ++ " (" ++ concat (intersperse ", " (map prettyExpr xs)) ++ "); \n" ++ (pretty ys)
pretty (Move x y :ys) = "Move " ++ (prettyExpr x) ++ ", " ++ (prettyExpr y) ++ "; \n" ++ (pretty ys)
pretty (Pen Up :ys) = "Pen up; \n" ++ (pretty ys)
pretty (Pen Down :ys) = "Pen down; \n" ++ (pretty ys)


prettyExpr :: Expr -> String
prettyExpr (Num x) = show x
prettyExpr (Refer n) = n
prettyExpr (Add s t) = prettyExpr s ++ "+" ++ prettyExpr t