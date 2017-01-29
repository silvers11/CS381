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

data Mode = Down
          | Up
   deriving (Eq,Show)

data Expr = Ref Var
          | Lit Num 
          | Add Expr Expr
   deriving (Eq,Show)

data Cmd = Pen Mode
         | Move Expr Expr
         | Define Macro [Var] Prog
         | Call Macro [Expr]
  deriving (Eq,Show)



