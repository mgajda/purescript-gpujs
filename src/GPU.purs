module GPU 
 ( module GPU
 , module GPU.DSL
 ) where

import GPU.DSL

foreign import data  Kernel :: Type -> Type

type Options o = { dimensions :: Array Int | o }

foreign import makeK0 :: ∀ o a. Options o -> String -> a
foreign import makeK1 :: ∀ o a b. Options o -> String -> Name -> a -> b
foreign import makeK2 :: ∀ o a b c. Options o -> String -> Name -> Name -> a -> b -> c
foreign import makeK3 :: ∀ o a b c d. Options o -> String -> Name -> Name -> Name -> a -> b -> c -> d
foreign import makeK4 :: ∀ o a b c d e. Options o -> String -> Name -> Name -> Name -> Name -> a -> b -> c -> d -> e
foreign import makeK5 :: ∀ o a b c d e f. Options o -> String -> Name -> Name -> Name -> Name -> Name -> a -> b -> c -> d -> e -> f
