module NBody where

import Math as Math
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

ssys = 1000
g = 6.674e-11
dt = 1e-4


randomSystem :: ∀ e. Eff (random :: RANDOM | e) System
randomSystem = 
  let coords = sequence <<< replicate ssys $ do
        q <- sequence <<< replicate 2 $ randomRange 0.0 1300.0 
        v <- sequence <<< replicate 2 $ randomRange (Ring.negate 1e2) 1e2
        pure [q, v]

      masses = sequence <<< replicate ssys $ randomRange 0.0 1e18
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

      pos x z = at3 coords x 0 z
      vel x z = at3 coords x 1 z
      mass = at masses

      x = 0
      y = 1

      coords = Variable "coords"
      masses = Variable "masses"
      body = Variable "body"

      calculatingVelocities = thready == 1

   in kernel2 opts masses coords $ do
        if' calculatingVelocities
          do v1 <- vset "v1" 0
             v2 <- vset "v2" 0
             d1 <- var "d1"
             d2 <- var "d2"
             r  <- var "r"
             va <- var "va"
             vr <- var "vr"

             for body 0 (Ring.sub ssys 1) $ 
               do if' (body == threadz) idle $ do
                   d1 <-- pos body x - pos threadz x
                   d2 <-- pos body y - pos threadz y
      
                   -- distance 
                   r <-- sqrt (sqr d1 + sqr d2)

                   -- partial Δv
                   va <-- dt * mass body * g / sqr r

                   -- sum components of Δv
                   v1 += va * (d1 / r)
                   v2 += va * (d2 / r)

             if' (threadx == x)
               do return (v1 + vel threadz x)
               do return (v2 + vel threadz y)

          -- calculating positions
          do if' (threadx == x)
               do return (pos threadz x + vel threadz x * dt)
               do return (pos threadz y + vel threadz y * dt)
