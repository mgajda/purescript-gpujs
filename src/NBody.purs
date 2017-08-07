module NBody where

import GPU 
import Data.Array (zipWith, length, replicate)
import Prelude hiding ((/=))
import Graphics.Canvas
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Random
import Data.Traversable
import Data.Maybe

type Vector = Array Number
type Matrix = Array Vector
type Time = Number

foreign import animate :: ∀ e. (System -> Eff e Unit) -> (System -> System) -> System -> Eff e Unit

main = do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  sys <- randomSystem

  animate (redraw ctx) (step 0.1) sys



randomSystem :: ∀ e. Eff (random :: RANDOM | e) System
randomSystem = sequence <<< replicate 100 $ do
  v <- sequence <<< replicate 2 $ randomRange 0.0 600.0 
  q <- sequence <<< replicate 2 $ randomRange 0.0 600.0 
  m <- randomRange 1.0 1e5
  pure { m:m, v:v, q:q }

redraw :: ∀ e. Context2D -> System -> Eff (canvas :: CANVAS | e) Unit
redraw ctx sys = do
  let qs = _.q <$> sys
      o [x,y] = { x:x, y:y, r:4.0, start:0.0, end:6.3 }
      o _ = { x:0.0, y:0.0, r:0.0, start:0.0, end:0.0 }

      drawO {q} = fillPath ctx $ arc ctx (o q)  

  _ <- clearRect ctx {x:0.0, y:0.0, h:600.0, w:800.0} 
  _ <- traverse drawO sys
  pure unit

type Body = {
    m :: Number
  , v :: Vector
  , q :: Vector
}

type System = Array Body

step :: Time -> Array Body -> Array Body
step dt sys = zipWith goback masses out
 where
    out = kernel masses velocities positions
    kernel = makeK3 opts  (show body) "masses" "velocities" "positions"
    
    opts = { dimensions: [100, 2, 2] }

    masses = _.m <$> sys
    velocities = _.v <$> sys
    positions = _.q <$> sys

    goback m [v, q] = { m:m, v:v, q:q }
    goback _ _ = { m:0.0, v:[0.0, 0.0], q:[0.0,0.0] }

    body = 
        vset "f1" zero
      : vset "f2" zero
      : var "d1" 
      : var "d2"
      : var "r"
      : vset "pf" zero
      : IF (ThreadY `Eq` zero) 
           (For "body" 0 (length sys)
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



body = show $
    vset "f1" zero
  : vset "f2" zero
  : var "d1" 
  : var "d2"
  : var "r"
  : vset "pf" zero
  : IF (ThreadY `Eq` zero) 
       (For "body" 0 100
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
           (return (at2 "positions" ThreadX zero + (at2 "velocities" ThreadX zero * Num 0.03)))
           (return (at2 "positions" ThreadX one  + (at2 "velocities" ThreadX one  * Num 0.03)))
       : end)
  : end
