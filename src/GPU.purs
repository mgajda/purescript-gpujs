module GPU 
 ( module GPU
 , module GPU.DSL
 ) where

import Prelude 
import GPU.DSL
import Data.Function.Uncurried

foreign import data  Kernel :: Type -> Type

type Options = {
    dimensions :: Array Int
  , debug :: Boolean
  , graphical :: Boolean
  , hardcodeConstants :: Boolean
  , outputToTexture :: Boolean
  , loopMaxIterations :: Int
  , mode :: String
}

def :: Options
def = {
    dimensions: [1]
  , debug: false
  , graphical: false
  , hardcodeConstants: false
  , outputToTexture: false
  , loopMaxIterations: 100
  , mode: "auto"
}

foreign import makeK0 :: ∀ a. Options -> String -> a
foreign import makeK1 :: ∀ a b. Options -> String -> Name -> a -> b
foreign import makeK2 :: ∀ a b c. Options -> String -> Name -> Name -> a -> b -> c
foreign import makeK3 :: ∀ a b c d. Options -> String -> Name -> Name -> Name -> a -> b -> c -> d
foreign import makeK4 :: ∀ a b c d e. Options -> String -> Name -> Name -> Name -> Name -> a -> b -> c -> d -> e
foreign import makeK5 :: ∀ a b c d e f. Options -> String -> Name -> Name -> Name -> Name -> Name -> a -> b -> c -> d -> e -> f
