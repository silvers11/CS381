-- Steven Silvers, silverss

module MiniLogo where

import Prelude hiding (Num)

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
          | Lit Num 
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