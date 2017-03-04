module Harmonics where

import Analysis

data Harmonic = Harmonic
    { harmCoefficient :: Double
    , harmFrequency   :: Double -- radians / x-unit
    , harmPhase       :: Double -- radians
    } deriving (Eq, Show)

data Series = Series
    { seriesOffset    :: Double
    , seriesHarmonics :: [Harmonic]
    } deriving (Eq, Show)

makeSeries :: Double -> [Double] -> [Double] -> [Double] -> Series
makeSeries o cs fs ps = Series o $ zipWith3 Harmonic cs fs ps

evaluate :: Series -> Double -> Double
evaluate (Series o hs) x = o + sum (map (`evaluate1` x) hs)
  where evaluate1 (Harmonic c s p) x' = c * cos (s * x' + p)

differentiate :: Series -> Series
differentiate (Series _ hs) = Series 0 (map differentiate1 hs)
  where differentiate1 (Harmonic c s p) = Harmonic (c * s) s (p + (pi/2))

type Point    = (Double, Double)
type Interval = (Double, Double)

-- TODO: chop up interval based on highest speed
extrema :: Series -> Double -> Interval -> [Extremum Point]
extrema s e (x0, x1) = [extremum f f' f'' e (x0, x1)]
  where s'  = differentiate s
        s'' = differentiate s'
        f   = evaluate s
        f'  = evaluate s'
        f'' = evaluate s''
