{-# LANGUAGE CPP #-}

module Main where

import Tides
import Analysis

import Control.Monad (forM_)
import Data.List
import Data.Ord
import Data.Time
import Data.Time.Locale.Compat
import Text.Printf

#if !MIN_VERSION_time(1,5,0)
parseTimeOrError _ = readTime
#endif

main = do
    let parseTime' :: ParseTime t => String -> String -> t
        parseTime' = parseTimeOrError True defaultTimeLocale
        toTime     = parseTime' "%F %H:%M"
        begin      = toTime "1700-01-01 00:00"
        end        = toTime "2101-01-01 00:00"
        step       = realToFrac (10 * 24 * 60 * 60)
        location   = "Hinkley"

    (_, times, _) <- tides location begin end step

    let (highs, lows) = partition ((== Maximum) . exType) times
        cmp = comparing (snd . exPoint)
        ranges = zipWith sub times $ tail times
        sub (Extremum (t, h) c) (Extremum (_, h') _) = Extremum (t, abs (h - h')) c

    forM_ (take 10 $ sortBy (flip cmp) highs) $
        putStrLn . formatEvent
    putStrLn "..."
    forM_ (reverse $ take 10 $ sortBy cmp lows) $
        putStrLn . formatEvent
    putStrLn "---"
    forM_ (take 10 $ sortBy (flip cmp) ranges) $
        putStrLn . formatEvent

formatPrediction :: Prediction -> String
formatPrediction (t, h) = printf "%s %9.6f" (formatTime defaultTimeLocale "%F %H:%M %Z" t) h
formatEvent :: Extremum Prediction -> String
formatEvent (Extremum p c) = formatPrediction p ++ printf " %-4s Tide" (fmtXtType c)
fmtXtType Maximum    = "High"
fmtXtType Minimum    = "Low"
fmtXtType Inflection = "Stationary" -- Never happens?
