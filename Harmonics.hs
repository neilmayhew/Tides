module Harmonics where

import Analysis

data Harmonic = Harmonic
    { harmCoeff :: Double
    , harmSpeed :: Double -- radians / x-unit
    , harmPhase :: Double -- radians
    } deriving (Eq, Show)

type Series = [Harmonic]

evaluate :: Series -> Double -> Double
evaluate hs x = sum $ map (`evaluate1` x) hs
  where evaluate1 (Harmonic c s p) x = c * cos (s * x + p)

differentiate :: Series -> Series
differentiate = map differentiate1
  where differentiate1 (Harmonic c s p) = Harmonic (c * s) s (p + (pi/2))

-- TODO: chop up interval based on highest speed
extrema :: [Harmonic] -> Double -> (Double, Double) -> [Extremum Double Double]
extrema hs e (x0, x1) = [extremum f f' f'' e (x0, x1)]
  where hs'  = differentiate hs
        hs'' = differentiate hs'
        f   = evaluate hs
        f'  = evaluate hs'
        f'' = evaluate hs''
