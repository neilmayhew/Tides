{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

#if IN_TEST_HARNESS
module TidesMain where

import Prelude hiding (IO, print, putStr, putStrLn)
import System.IO.Fake
#endif

import Tides
import Analysis

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Time
import Data.Time.Locale.Compat (TimeLocale, defaultTimeLocale)
import System.Environment
import System.Exit
import Text.Printf

#if !MIN_VERSION_time(1,5,0)
parseTimeOrError :: ParseTime t => Bool -> TimeLocale -> String -> String -> t
parseTimeOrError _ = readTime
#endif

main :: IO ()
main = liftIO getArgs >>= \case

  [location, begin, end, step] -> do

    let parseTime' :: ParseTime t => String -> String -> t
        parseTime' = parseTimeOrError True defaultTimeLocale
        toTime     = parseTime' "%F %H:%M"
        toInterval = realToFrac . timeOfDayToTime . parseTime' "%H:%M"

    (heights, events, units, _) <- tides location (toTime begin) (toTime end) (toInterval step)

    -- Output tide heights for the period

    forM_ heights $ \(t, h) ->
        putStrLn $ printf "%s %10.6f" (showTime t) h

    -- Output low and high tides for the period

    forM_ events $ \(Extremum (t, h) c) ->
        putStrLn $ printf "%s %6.2f %s  %s Tide"
            (showTime t) h units (showType c)

  _ -> liftIO $ die . printf "Usage: %s LOCATION BEGIN END STEP" =<< getProgName

  where
    showTime :: ZonedTime -> String
    showTime t = formatTime defaultTimeLocale "%F %l:%M %p %Z" t'
      where t' = addZonedTime 30 t -- Display nearest minute
    addZonedTime s t = utcToZonedTime (zonedTimeZone t) (s `addUTCTime` zonedTimeToUTC t)
    showType Maximum    = "High"
    showType Minimum    = "Low"
    showType Inflection = "Stationary" -- Never happens?
