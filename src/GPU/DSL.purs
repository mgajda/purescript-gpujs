module GPU.DSL
   where

import Data.Array (intercalate)
import Prelude
import Data.Foldable (foldMap)
import Data.Exists (mkExists, runExists, Exists)

class Render a where 
  render :: a -> String

type Block = Array Statement

data Expr =
    Num  Number
  | Inum Int
  | Read String
  | Call String
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Mod Expr Expr
  | Ind1 String Expr
  | Ind2 String Expr Expr
  | Ind3 String Expr Expr Expr
  | App1 String Expr
  | App2 String Expr Expr
  | App3 String Expr Expr Expr

data Cond = 
    Gt Expr Expr
  | Ge Expr Expr
  | Lt Expr Expr
  | Le Expr Expr
  | Eq Expr Expr
  | Ne Expr Expr
  | And Cond Cond
  | Or  Cond Cond
  | Not Cond
  | True
  | False

data Statement = 
    Var String
  | Set String Expr
  | Vset String Expr
  | For String Int Int Block
  | IF Cond Block Block
  | While Cond Block
  | Function String (Array String) Block
  | Return Expr
  | Break
  | Idle

instance showCond :: Render Cond where
  render = case _ of
    Gt a b -> render a <> " > " <> render b
    Ge a b -> render a <> " >= " <> render b
    Lt a b -> render a <> " < " <> render b
    Le a b -> render a <> " <= " <> render b
    Eq a b -> render a <> " == " <> render b
    Ne a b -> render a <> " != " <> render b
    And a b -> render a <> " && " <> render b
    Or  a b -> render a <> " || " <> render b
    Not a  -> "!(" <> render a <> ")"
    True  -> "true"
    False -> "false"
  
instance showBlock :: Render a => Render (Array a) where
  render block = "{ " <> foldMap go block <> " }"
    where 
      go stm = " " <> render stm <> ";"

instance showStm :: Render Statement where
  render = case _ of 
    Break          -> "break"
    Idle           -> ""
    Return e       -> "return " <> render e
    Var n          -> "var " <> n <> ";"
    Set n e        -> n <> " = " <> render e
    Vset n s       -> "var " <> n <> " = " <> render s <> ";"
    For index from to body  -> "for (var " <> index <> " = " <> show from <> "; " <> index <> "<= " <> show to <> "; " <> index <> "++) " <> render body
    IF cond pos neg         -> "if (" <> render cond <> ") " <> render pos <> " else " <> render neg
    While cond body         -> "while (" <> render cond <> ") " <> render body
    Function name [] body   -> "function " <> name <> " () " <> render body
    Function name args body -> "function " <> name <> " ("<> intercalate ", " args  <>") " <> render body

instance showExpr :: Render Expr where
  render = case _ of
    Add a b -> render a <> " + " <> render b
    Sub a b -> render a <> " - " <> render b
    Mul a b -> render a <> " * " <> render b
    Div a b -> render a <> " / " <> render b
    Mod a b -> render a <> " % " <> render b

    Call st -> st <> "()"
    Num n -> show n
    Inum n -> show n
    Read n -> n

    App1 n e -> n <> "(" <> render e  <> ")"
    App2 n e1 e2 -> n <> "(" <> render e1 <> ", " <> render e2 <> ")"
    App3 n e1 e2 e3 -> n <> "(" <> render e1 <> ", " <> render e2 <> ", " <> render e3 <> ")"

    Ind1 n e -> n <> "[" <> render e <> "]"
    Ind2 n e1 e2 -> n <> "[" <> render e1 <> "][" <> render e2 <> "]"
    Ind3 n e1 e2 e3 -> n <> "[" <> render e1 <> "][" <> render e2 <> "][" <> render e3 <> "]"

