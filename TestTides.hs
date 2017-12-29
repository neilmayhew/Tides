{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main where

import Tides
import Time
import Analysis

import Control.Arrow (second)
import Control.Monad
import Data.Bool (bool)
import Data.Functor ((<$>))
import Data.Time
import Data.Time.Locale.Compat (TimeLocale, defaultTimeLocale)
import HSH
import System.Environment
import System.Exit
import System.Random
import Text.Printf

#if !MIN_VERSION_time(1,5,0)
parseTimeOrError :: ParseTime t => Bool -> TimeLocale -> String -> String -> t
parseTimeOrError _ = readTime
readSTime :: ParseTime t => Bool -> TimeLocale -> String -> ReadS t
readSTime _ = readsTime
#endif

main :: IO ()
main = do
    (location:args) <- getArgs

    interval <- randomIO

    let zone = utc -- TODO: get from location
        toLocal = clampTime . utcToLocalTime zone
        clampTime (LocalTime d t) = LocalTime d t { todSec = 0 }
        parseTime' :: ParseTime t => String -> String -> t
        parseTime' = parseTimeOrError True defaultTimeLocale
        toTime     = parseTime' "%F %H:%M"
        toInterval = realToFrac . timeOfDayToTime . parseTime' "%H:%M"

        (begin, end, step) = if null args
            then (toLocal $ prBegin interval, toLocal $ prEnd interval, prStep interval)
            else (toTime $ args!!0, toTime $ args!!1, toInterval $ args!!2)

    when (null args) $
        putStrLn $ unwords [show begin, show end, show . timeToTimeOfDay . nominalToTime $ step]

    modelPredictions <- getModelPredictions location begin end step
    modelEvents      <- getModelEvents      location begin end

    (predictions, events, _) <- tides location begin end step

    let predictionPairs = zip modelPredictions predictions
        eventPairs      = zip modelEvents events

        eqPred  (t, h) (t', h') = eqTime t t' && abs (h - h') < 1e-6
        eqEvent (Extremum (t, h) c) (Extremum (t', h') c') = (c == c') && eqTime t t' && abs (h - h') < 0.006
        eqTime t t' = ztzName t == ztzName t' && abs (ztUTC t `diffUTCTime` ztUTC t') < 60
          where ztzName = timeZoneName . zonedTimeZone
                ztUTC x = zonedTimeToUTC x { zonedTimeZone = utc }

        predictionMismatches = filter (not . uncurry eqPred ) predictionPairs
        eventMismatches      = filter (not . uncurry eqEvent) eventPairs

    unless (null predictionMismatches) $
        putStrLn "Predictions don't match"

    forM_ predictionMismatches $ \(a, b) ->
        putStrLn $ formatPrediction a ++ " / " ++ formatPrediction b

    unless (null eventMismatches) $
        putStrLn "Events don't match"

    forM_ eventMismatches $ \(a, b) ->
        putStrLn $ formatEvent a ++ " / " ++ formatEvent b

    bool exitFailure exitSuccess $ null predictionMismatches && null eventMismatches

formatPrediction :: Prediction -> String
formatPrediction (t, h) = printf "%s %9.6f" (formatTime defaultTimeLocale "%F %H:%M:%S %Z" t) h
formatEvent :: Event -> String
formatEvent (Extremum p c) = formatPrediction p ++ printf " %-4s Tide" (fmtXtType c)

getModelPredictions :: String -> LocalTime -> LocalTime -> NominalDiffTime -> IO [Prediction]
getModelPredictions location begin end step = do
    let cmd = tideCmd location begin end (Just step) "m"
    map parseLine <$> run cmd
  where
    parseLine = second read . parseXtTime

getModelEvents :: String -> LocalTime -> LocalTime -> IO [Event]
getModelEvents location begin end = do
    let cmd = tideCmd location begin end Nothing "p" ++ " | sed '1,/^$/d'"
    map parseLine <$> run cmd
  where
    parseLine s = Extremum (t, h) c
      where (t, rest) = parseXtTime s
            [ht, _, ty, _] = words rest
            h = read ht
            c = parseXtType ty

tideCmd :: String -> LocalTime -> LocalTime -> Maybe NominalDiffTime -> String -> String
tideCmd location begin end step mode =
    printf "tide -l '%s' -b '%s' -e '%s' -em pSsMm -m %s 2>/dev/null"
        location (fmtXtTime begin) (fmtXtTime end) mode
        ++ maybe "" (printf " -s '%s'" . fmtXtInterval) step

fmtXtTime     :: LocalTime -> String
fmtXtTime     = formatTime     defaultTimeLocale "%F %H:%M"
fmtXtInterval :: NominalDiffTime -> String
fmtXtInterval = formatTime     defaultTimeLocale "%H:%M" . timeToTimeOfDay . realToFrac
readsXtTime   :: ReadS ZonedTime
readsXtTime   = readSTime True defaultTimeLocale "%F %l:%M %p %Z"
parseXtTime   :: String -> (ZonedTime, String)
parseXtTime s = case readsXtTime s of
    [x] -> x
    _   -> error $ "Can't parse time: " ++ s
fmtXtType :: Criticality -> String
fmtXtType Maximum    = "High"
fmtXtType Minimum    = "Low"
fmtXtType Inflection = "Stationary" -- Never happens?
parseXtType :: String -> Criticality
parseXtType t = case t of
    "Low" -> Minimum
    "High" -> Maximum
    _ -> error $ "Unknown tide type: " ++ t

data PredictionInterval = PredictionInterval
    { prBegin :: UTCTime
    , prEnd   :: UTCTime
    , prStep  :: NominalDiffTime
    } deriving (Eq, Show)

instance Random PredictionInterval where
    random = randomR (PredictionInterval periodStart periodEnd 0, PredictionInterval periodStart periodEnd 0)
      where -- Period supported by current tide component database is 1700-2101
            -- However, date code seems to have problems outside 1848-2037
            periodStart = UTCTime (fromGregorian 1848 1 1) 0
            periodEnd   = UTCTime (fromGregorian 2037 1 1) (-1)
    randomR (lo, hi) g = (PredictionInterval begin end step, g''')
      where (begin,   g'  ) = randomR (prBegin lo, prEnd hi) g
            (minutes, g'' ) = randomR (1, 120) g'
            (number,  g''') = randomR (3, 97) g''
            step = realToFrac (minutes * 60 :: Int)
            end  = min (prEnd hi) $ (number * step) `addUTCTime` begin
