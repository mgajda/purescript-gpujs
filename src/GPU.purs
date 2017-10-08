module GPU 
  ( module GPU
  , module GPU.ALT
  )
  where

import Control.Monad.Writer (execWriter)
import Prelude ((<<<))
import GPU.ALT
import GPU.DSL
import Data.Foldable (foldMap)

foreign import data  Kernel :: Type -> Type

type Options o = { output :: Array Int | o }

foreign import makeK0 :: ∀ o a. Options o -> String -> a
foreign import makeK1 :: ∀ o a b. Options o -> String -> String -> a -> b
foreign import makeK2 :: ∀ o a b c. Options o -> String -> String -> String -> a -> b -> c
foreign import makeK3 :: ∀ o a b c d. Options o -> String -> String -> String -> String -> a -> b -> c -> d
foreign import makeK4 :: ∀ o a b c d e. Options o -> String -> String -> String -> String -> String -> a -> b -> c -> d -> e

showBody :: Block -> String
showBody = foldMap render

kernel0 :: ∀ o a. Options o -> Script -> a
kernel0 opts = makeK0 opts <<< showBody <<< execWriter

kernel1 :: ∀ o a b. Options o -> Variable -> Script -> a -> b
kernel1 opts (Variable p1) = makeK1 opts p1 <<< showBody <<< execWriter

kernel2 :: ∀ o a b c. Options o -> Variable -> Variable -> Script -> a -> b -> c
kernel2 opts (Variable p1) (Variable p2) = makeK2 opts p1 p2 <<< showBody <<< execWriter

kernel3 :: ∀ o a b c d. Options o -> Variable -> Variable -> Variable -> Script -> a -> b -> c -> d
kernel3 opts (Variable p1) (Variable p2) (Variable p3) = makeK3 opts p1 p2 p3 <<< showBody <<< execWriter

kernel4 :: ∀ o a b c d e. Options o -> Variable -> Variable -> Variable -> Variable -> Script -> a -> b -> c -> d -> e
kernel4 opts (Variable p1) (Variable p2) (Variable p3) (Variable p4) = makeK4 opts p1 p2 p3 p4 <<< showBody <<< execWriter 
