module Test where

import Data.Array (length, (!!))
import Data.Maybe (Maybe (..))
import GPU
import Prelude (class Show, show, discard, bind, ($))
import Data.Ring as Ring

def :: { output :: Array Int }
def = { output: [1] }

identity :: ∀ a. Show a => a -> a
identity = 
  let x = Variable "x"
   in kernel1 def x $ do
        return x


testMultMatrix :: String
testMultMatrix = show $ a `matMult` b
  where
      a = [ [1, 1, 3]
          , [0, 1, 3] 
          , [12, 12, 12]
          ]

      b = [ [0, 1, 3]
          , [1, 1, 3] 
          , [1, 1, 3] 
          ]

type Matrix = Array (Array Int)

testIf :: Number -> Number -> Array Number
testIf = 
  let a = Variable "a"
      b = Variable "b"
   in kernel2 def a b $ do
        res <- var "res"
        if' (a > 23) 
          do res <-- a
          do res <-- b
        return res

matMult :: Matrix -> Matrix -> Matrix
matMult n m = 
  let a = Variable "a"
      b = Variable "b"

      d2 = length n
      d1 = case (m !! 0) of
                Just x -> length x
                Nothing -> 0

      opts = def { output = [d1, d2] }

      i = Variable "i"

      kern = kernel2 opts a b $ do
        sum <- vset "sum" 0
        for i 0 (Ring.sub d1 1) $
          sum += at2 a thready i * at2 b i threadx
        return sum

   in kern n m
