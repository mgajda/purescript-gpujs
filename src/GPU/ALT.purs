module GPU.ALT where

import Data.Tuple
import Prelude ((*>), bind, (<$>), (<*), (=<<), ($), pure, Unit, (<<<), id, show)
import Data.Foldable (traverse_)
import Data.Array (singleton)
import GPU.DSL
import Control.Monad.Writer

type Binary v = ∀ a b. ToExpr a => ToExpr b => (Expr -> Expr -> v) -> a -> b -> v
type Unary  v = ∀ a.   ToExpr a => (Expr -> v) -> a -> v

bie :: Binary Expr
bie f a b = f (toExpr a) (toExpr b)

bic :: Binary Cond
bic f a b = f (toExpr a) (toExpr b)

un :: Unary Expr
un f a = f (toExpr a)

class ToExpr a where
  toExpr :: a -> Expr

newtype Variable = Variable String
instance variableToExpression :: ToExpr Variable
  where toExpr (Variable name) = Read name

instance eint :: ToExpr Int 
  where toExpr = Inum
instance enum :: ToExpr Number
  where toExpr = Num
instance eval :: ToExpr Expr
  where toExpr = id

type Val = ∀ a. ToExpr a => a

-- numeric operations

add = bie Add 
sub = bie Sub
mul = bie Mul
div = bie Div
mod = bie Mod

-- ordering 

gt = bic Gt
ge = bic Ge
lt = bic Lt
le = bic Le
eq = bic Eq
ne = bic Ne

not = Not

-- infix synonyms

infixl 4 gt as >
infixl 4 ge as >=
infixl 4 lt as <
infixl 4 le as <=
infixl 4 eq as ==
infixl 4 ne as !=

infixr 3 And as &&
infixr 2 Or  as ||

infixl 6 add as +
infixl 6 sub as -
infixl 7 mul as *
infixl 7 div as /
infixl 6 mod as %

-- numric assignments

setplus :: ∀ e. ToExpr e => Variable -> e -> Script
setplus v e = v <-- v + e

setminus :: ∀ e. ToExpr e => Variable -> e -> Script
setminus v e = v <-- v - e

setmult :: ∀ e. ToExpr e => Variable -> e -> Script
setmult v e = v <-- v * e

setdiv :: ∀ e. ToExpr e => Variable -> e -> Script
setdiv v e = v <-- v / e

infixr 1 setplus as +=
infixr 1 setminus as -=
infixr 1 setmult as *=
infixr 1 setdiv as /=

type Return = Writer Block
type Script = Writer Block Unit

var :: String -> Return Variable
var name = tell [Var name] *> pure (Variable name)

set :: ∀ e. ToExpr e => Variable -> e -> Return Unit
set (Variable name) e = tell [Set name $ toExpr e]

vset :: ∀ e. ToExpr e => String -> e -> Return Variable
vset name expr = tell [Vset name $ toExpr expr] *> pure (Variable name)

for :: Variable -> Int -> Int -> Script -> Script
for (Variable name) from to abody = 
  tell <<< singleton <<< For name from to $ execWriter abody

while :: Cond -> Script -> Script
while cond body =
  tell <<< singleton <<< While cond $ execWriter body

if' :: Cond -> Script -> Script -> Script 
if' cond pos neg = tell (singleton $ IF cond p n)
  where
    p = execWriter pos 
    n = execWriter neg

function name (Variable p1) body =
  (tell <<< singleton <<< Function name [p1] <<< execWriter $ body)
  *> pure (apply1 name)

function2 name (Variable p1) (Variable p2) body =
  (tell <<< singleton <<< Function name [p1, p2] <<< execWriter $ body)
  *> pure (apply2)

function3 name (Variable p1) (Variable p2) (Variable p3) body =
  (tell <<< singleton <<< Function name [p1, p2, p3] <<< execWriter $ body)
  *> pure (apply3 name)
  
return :: ∀ e. ToExpr e => e -> Script
return = tell <<< singleton <<< Return <<< toExpr

break :: Script
break = tell [Break]

at :: ∀ a. ToExpr a => Variable -> a -> Expr
at (Variable var) = Ind1 var <<< toExpr

at2 :: ∀ a b. ToExpr a => ToExpr b => Variable -> a -> b -> Expr
at2 (Variable var) a b = Ind2 var (toExpr a) (toExpr b)

at3 :: ∀ a b c. ToExpr a => ToExpr b => ToExpr c => Variable -> a -> b -> c -> Expr
at3 (Variable var) a b c = Ind3 var (toExpr a) (toExpr b) (toExpr c)

call :: Variable -> Expr
call (Variable var) = Call var

apply1 :: ∀ a. ToExpr a => String -> a -> Expr
apply1 n a = App1 n (toExpr a)

apply2 :: ∀ a b. ToExpr a => ToExpr b => String -> a -> b -> Expr
apply2 n a b = App2 n (toExpr a) (toExpr b)

apply3 :: ∀ a b c. ToExpr a => ToExpr b => ToExpr c => String -> a -> b -> c -> Expr
apply3 n a b c = App3 n (toExpr a) (toExpr b) (toExpr c)

sqr :: ∀ a. ToExpr a => a -> Expr
sqr e = apply2 "Math.pow" e 2

sqrt :: ∀ a. ToExpr a => a -> Expr
sqrt = App1 "Math.sqrt" <<< toExpr

threadx = Variable "this.thread.x"
thready = Variable "this.thread.y"
threadz = Variable "this.thread.z"

infixr 1 set as <--
infixr 2 if' as ?
