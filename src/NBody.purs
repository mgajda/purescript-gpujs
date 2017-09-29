module NBody where

import GPU 
import Data.Array (zipWith, length, replicate)
import Prelude hiding ((/=))
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Random
import Control.Monad.Eff.Console
import Data.Traversable
import Data.Maybe

import Signal
import Signal.Time (every)

type Vector = Array Number
type Matrix = Array Vector

foreign import putCircle :: ∀ e. Body -> Eff e Unit
foreign import clearSvg :: ∀ e. Eff e Unit

main = do
  sys <- randomSystem
  runSignal $ redraw <~ foldp ($) sys (sampleOn (every 0.33) (constant step))


randomSystem :: ∀ e. Eff (random :: RANDOM | e) System
randomSystem = sequence <<< replicate 100 $ do
  v <- sequence <<< replicate 2 $ randomRange 0.0 600.0 
  q <- sequence <<< replicate 2 $ randomRange 0.0 600.0 
  m <- randomRange 1.0 1e-2
  pure { m:m, v:v, q:q }

redraw :: ∀ e. System -> Eff e Unit
redraw sys = traverse_ putCircle sys

type Body = {
    m :: Number
  , v :: Vector
  , q :: Vector
}

type System = Array Body

step :: Array Body -> Array Body
step sys = zipWith goback masses out
 where
    out = kernel masses velocities positions
    
    masses = _.m <$> sys
    velocities = _.v <$> sys
    positions = _.q <$> sys

    goback m [v, q] = { m:m, v:v, q:q }
    goback _ _ = { m:0.0, v:[0.0, 0.0], q:[0.0,0.0] }

type MyKern = Array Number -> Array (Array Number) -> Array (Array Number) -> Array (Array (Array Number))

kernel :: MyKern
kernel = makeK3 opts (show body) "masses" "velocities" "positions"
  where
    opts = {
        output: [ 2, 2, ssys ]
      , mode: "gpu"
      }

    dt = 0.1
    ssys = 100

    body = 
        vset "f1" zero
      : vset "f2" zero
      : var "d1" 
      : var "d2"
      : var "r"
      : vset "pf" zero
      : IF (ThreadY `Eq` zero) 
           (For "body" 0 ssys
                ( IF (read "body" `Eq` ThreadX) break
                     ( "d1" <-- at2 "positions" (read "body") zero - at2 "positions" ThreadX zero
                     : "d2" <-- at2 "positions" (read "body") one  - at2 "positions" ThreadX one
                     : "r"  <-- (sqrt (sqr (read "d1") + sqr (read "d2")))
                     : "d1" <-- read "d1" / read "r"
                     : "d2" <-- read "d2" / read "r"
                     : "pf" <-- at "masses" (read "body") * at "masses" ThreadX * (Num 6.674e-11) / sqr (read "r")
                     : "f1" <-- read "f1" + read "d1" * read "pf"
                     : "f2" <-- read "f2" + read "d2" * read "pf"
                     : end )
                : IF (ThreadZ `Eq` zero) 
                     (return (read "f1" / at "masses" ThreadX))
                     (return (read "f2" / at "masses" ThreadX))
                : end)
           : end)
           (IF (ThreadZ `Eq` zero)
               (return (at2 "positions" ThreadX zero + (at2 "velocities" ThreadX zero * Num dt)))
               (return (at2 "positions" ThreadX one  + (at2 "velocities" ThreadX one  * Num dt)))
           : end)
      : end
