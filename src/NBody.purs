module NBody where

import GPU 
import Data.Array (zipWith, length, replicate)
import Prelude (unit, (<<<), ($), Unit, bind, discard)
import Data.Ring as Ring
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Random
import Control.Monad.Eff.Console
import Data.Traversable (sequence)
import Data.Maybe

type Vector = Array Number
type Matrix = Array Vector

foreign import animate :: ∀ e. (System -> System) -> System -> Eff e Unit

main = do
  sys <- randomSystem
  pure unit
  animate step sys

randomSystem :: ∀ e. Eff (random :: RANDOM | e) System
randomSystem = 
  let coords = sequence <<< replicate 100 $ do
        v <- sequence <<< replicate 2 $ randomRange 0.0 600.0 
        q <- sequence <<< replicate 2 $ randomRange 0.0 600.0 
        pure [q, v]

      masses = sequence <<< replicate 100 $ randomRange 1.0 1e2
   in { masses: _, coords: _ } <$> masses <*> coords

type System = { coords :: Array (Array (Array Number)), masses :: Array Number }

step :: System -> System
step sys = sys { coords = kernel sys.masses sys.coords }

type MyKern = Array Number -> Array (Array (Array Number)) -> Array (Array (Array Number))

kernel :: MyKern
kernel = 
  let opts = {
          output: [ 2, 2, ssys ]
        , mode: "gpu"
        }

      dt = 0.031
      ssys = 100

      pos b c = at3 coords b 0 c
      vel b c = at3 coords b 1 c
      mass = at masses

      x = 0
      y = 1

      coords = Variable "coords"
      masses = Variable "masses"
      body = Variable "body"

      calculatingVelocities = thready == 1

   in kernel2 opts masses coords $ do
        v1 <- vset "v1" 0
        v2 <- vset "v2" 0
        d1 <- var "d1"
        d2 <- var "d2"
        r  <- var "r"
        v  <- var "v"

        if' calculatingVelocities
          do for body 0 (Ring.sub ssys 1) $ 
               do if' (body == threadx) break $ do
                   d1 <-- pos body x - pos threadx x
                   d2 <-- pos body y - pos threadx y
      
                   r <-- sqrt (sqr d1 + sqr d2)
                   v <-- dt * mass body * 6.674e-11 / sqr r
      
                   v1 += v * d1 / r
                   v2 += v * d2 / r

             if' (threadz == x)
               do return v1
               do return v2

          do if' (threadz == x)
               do return (pos threadx x + vel threadx x * dt)
               do return (pos threadx y + vel threadx y * dt)
