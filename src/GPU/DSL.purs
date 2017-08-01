module GPU.DSL
   where

import Prelude

type Name = String

-- exprssions must always be of type Float32
data Expression =
    Num Number
  | Call Name
  | Read Name
  | Apply1 Name Expression
  | Apply2 Name Expression Expression
  | Apply3 Name Expression Expression Expression
  | Index1 Name Expression
  | Index2 Name Expression Expression 
  | Index3 Name Expression Expression Expression
  | ThreadX
  | ThreadY
  | ThreadZ
  | DimensionsX
  | DimensionsY
  | DimensionsZ
  | Add Expression Expression
  | Sub Expression Expression
  | Mul Expression Expression
  | Div Expression Expression
  | Mod Expression Expression

instance exprSemiRing :: Semiring Expression where
  one = Num 1.0
  add = Add
  zero = Num 0.0
  mul = Mul

instance exprRing :: Ring Expression where
  sub = Sub

instance exprCommRing :: CommutativeRing Expression
instance exprEuclRing :: EuclideanRing Expression where
  mod = Mod
  div = Div
  degree _ = 1

data Cond = 
    Gt Expression Expression
  | Ge Expression Expression
  | Lt Expression Expression
  | Le Expression Expression
  | Eq Expression Expression
  | Ne Expression Expression
  | And Cond Cond
  | Or  Cond Cond
  | Not Cond
  | True
  | False

instance condHeyting :: HeytingAlgebra Cond where
  not = Not
  conj = And
  disj = Or
  implies p q = not p || q 
  tt = True
  ff = False

instance condBool :: BooleanAlgebra Cond

data Statement = 
    Var Name
  | Assign Name Expression
  | For Name Int Int Block
  | IF Cond Block Block
  | While Cond Block

data Block = S Statement Block | Return Expression | End

instance showCond :: Show Cond where
  show = case _ of
    Gt a b -> show a <> " > " <> show b
    Ge a b -> show a <> " >= " <> show b
    Lt a b -> show a <> " < " <> show b
    Le a b -> show a <> " <= " <> show b
    Eq a b -> show a <> " == " <> show b
    Ne a b -> show a <> " != " <> show b
    And a b -> show a <> " && " <> show b
    Or  a b -> show a <> " || " <> show b
    Not a  -> "!(" <> show a <> ")"
    True  -> "true"
    False -> "false"
  
instance showBlock :: Show Block where
  show block = "{ " <> go block <> " }"
    where 
      go = case _ of 
        S stm rest -> show stm <> "; " <> go rest
        Return res -> "return " <> show res <> ";"
        End  -> ""

instance showStm :: Show Statement where
  show = case _ of 
    Var n          -> "var " <> n
    Assign n expr  -> n <> " = " <> show expr
    For index from to body  -> "for (var " <> index <> " = " <> show from <> "; i <= " <> show to <> "; i++) " <> show body
    IF cond pos neg         -> "if (" <> show cond <> ") " <> show pos <> " else " <> show neg
    While cond body         -> "while (" <> show cond <> ") " <> show body

instance showExpr :: Show Expression where
  show = case _ of
    Num n -> show n
    Read n             -> n
    Call n             -> n <> "()"
    Apply1 n e         -> n <> "(" <> show e  <> ")"
    Apply2 n e1 e2     -> n <> "(" <> show e1 <> ", " <> show e2 <> ")"
    Apply3 n e1 e2 e3  -> n <> "(" <> show e1 <> ", " <> show e2 <> ", " <> show e3 <> ")"
    Index1 n e         -> n <> "[" <> show e  <> "]"
    Index2 n e1 e2     -> n <> "[" <> show e1 <> "][" <> show e2 <> "]"
    Index3 n e1 e2 e3  -> n <> "[" <> show e1 <> "][" <> show e2 <> "][" <> show e3 <> "]"
    ThreadX -> "this.thread.x"
    ThreadY -> "this.thread.y"
    ThreadZ -> "this.thread.z"
    DimensionsX -> "this.dimensions.x"
    DimensionsY -> "this.dimensions.y"
    DimensionsZ -> "this.dimensions.z"

    Add a b -> show a <> " + " <> show b
    Sub a b -> show a <> " - " <> show b
    Mul a b -> show a <> " * " <> show b
    Div a b -> show a <> " / " <> show b
    Mod a b -> show a <> " % " <> show b

var :: Name -> Statement
var = Var 

set :: Name -> Expression -> Statement
set = Assign 

return :: Expression -> Block
return = Return

read :: Name -> Expression
read = Read 

end :: Block
end = End

at :: Name -> Expression -> Expression
at  = Index1 

at2 :: Name -> Expression -> Expression -> Expression
at2 = Index2 

at3 :: Name -> Expression -> Expression -> Expression -> Expression
at3 = Index3 

i = read "i"

call :: Name -> Expression
call = Call 

infixr 0 S as :
