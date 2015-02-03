module Main where

import Tides
import Time
import Analysis

import Control.Monad (forM_)
import Data.List
import Data.Ord
import Data.Time
import System.Locale (defaultTimeLocale)
import Text.Printf

main = do
    let readTime' = readTime defaultTimeLocale "%F %H:%M"
        begin = readTime' "1700-01-01 00:00"
        end   = readTime' "2101-01-01 00:00"
        step  = realToFrac (10 * 24 * 60 * 60)
        location = "Hinkley"

    (_, times, _) <- tides location begin end step

    let (highs, lows) = partition ((== Maximum) . exType) times
        cmp = comparing (snd . exPoint)

    forM_ (take 10 $ sortBy (flip cmp) highs) $
        putStrLn . formatEvent
    putStrLn "..."
    forM_ (reverse $ take 10 $ sortBy cmp lows) $
        putStrLn . formatEvent

formatPrediction :: Prediction -> String
formatPrediction (t, h) = printf "%s %9.6f" (formatTime defaultTimeLocale "%F %H:%M %Z" t) h
formatEvent :: Extremum Prediction -> String
formatEvent (Extremum p c) = formatPrediction p ++ printf " %-4s Tide" (fmtXtType c)
fmtXtType Maximum    = "High"
fmtXtType Minimum    = "Low"
fmtXtType Inflection = "Stationary" -- Never happens?
