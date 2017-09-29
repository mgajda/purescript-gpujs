module Test where

import Data.Array (length, (!!))
import Data.Maybe (Maybe (..))
import GPU
import Prelude 

def = { output: [1] }

identity :: ∀ a. Show a => a -> a
identity = makeK1 opts body "x"
  where opts = def
        body = show (return $ read "x")
  

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
testIf = makeK2 def (show body) "a" "b"
  where
    body = 
        var "res" 
      : IF (read "a" `Gt` Num 23.0) 
           (set "res" (read "a") : end)
           (set "res" (read "b") : end)
      : return (read "res")

matMult :: Matrix -> Matrix -> Matrix
matMult n m = makeK2 opts (show body) "A" "B" n m
  where
    opts = def { output = [d1, d2] }

    d2 = length n
    d1 = case (m !! 0) of
              Just x -> length x
              Nothing -> 0

    i = read "i"
    body =
        var "sum"
      : set "sum" zero
      : For "i" 0 (d1 - 1)
          (set "sum" (read "sum" + at2 "A" ThreadY i * at2 "B" i ThreadX) : end)
      : return (read "sum") 
