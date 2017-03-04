{-# LANGUAGE CPP #-}

module Main where

import Tides
import Analysis

import Control.Monad
import Data.Time
import Data.Time.Locale.Compat
import System.Environment
import Text.Printf

#if !MIN_VERSION_time(1,5,0)
parseTimeOrError _ = readTime
#endif

main = do
    [location, begin, end, step] <- getArgs

    let parseTime' :: ParseTime t => String -> String -> t
        parseTime' = parseTimeOrError True defaultTimeLocale
        toTime     = parseTime' "%F %H:%M"
        toInterval = realToFrac . timeOfDayToTime . parseTime' "%H:%M"

    (heights, events, units) <- tides location (toTime begin) (toTime end) (toInterval step)

    -- Output tide heights for the period

    forM_ heights $ \(t, h) ->
        putStrLn $ printf "%s %10.6f" (showTime t) h

    -- Output low and high tides for the period

    forM_ events $ \(Extremum (t, h) c) ->
        putStrLn $ printf "%s %6.2f %s  %s Tide"
            (showTime t) h units (showType c)
  where
    showTime :: ZonedTime -> String
    showTime t = formatTime defaultTimeLocale "%F %l:%M %p %Z" t'
      where t' = addZonedTime 30 t -- Display nearest minute
    addZonedTime s t = utcToZonedTime (zonedTimeZone t) (s `addUTCTime` zonedTimeToUTC t)
    showType Maximum    = "High"
    showType Minimum    = "Low"
    showType Inflection = "Stationary" -- Never happens?
