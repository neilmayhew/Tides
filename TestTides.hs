module Main where

import Time
import Analysis

import Control.Arrow (first, second)
import Control.Monad
import Data.Functor
import Data.Time
import HSH
import System.Environment
import System.Locale (defaultTimeLocale)
import System.Random
import Text.Printf

main = do
    (location:args) <- getArgs

    interval <- randomIO

    let zone = utc -- TODO: get from location
        toLocal = utcToLocalTime zone
        readTime'    = readTime defaultTimeLocale "%F %H:%M"
        readInterval = realToFrac . timeOfDayToTime . readTime defaultTimeLocale "%H:%M"

        (begin, end, step) = if null args
            then (toLocal $ prBegin interval, toLocal $ prEnd interval, prStep interval)
            else (readTime' $ args !! 0, readTime' $ args !! 1, readInterval $ args !! 2)

    heights <- getModelHeights location begin end step

    forM_ heights $
        putStrLn . formatPrediction

    times <- getModelTimes location begin end

    forM_ times $
        putStrLn . formatEvent

type Prediction = (ZonedTime, Double)

formatPrediction :: Prediction -> String
formatPrediction (t, h) = printf "%s %9.6f" (formatTime defaultTimeLocale "%F %H:%M %Z" t) h
formatEvent :: Extremum Prediction -> String
formatEvent (Extremum p c) = formatPrediction p ++ printf " %-4s Tide" (fmtXtType c)

getModelHeights :: String -> LocalTime -> LocalTime -> NominalDiffTime
                   -> IO [Prediction]
getModelHeights location begin end step = do
    let cmd = tideCmd location begin end (Just step) "m"
    map parseLine <$> run cmd
  where
    parseLine = second read . parseXtTime

getModelTimes :: String -> LocalTime -> LocalTime
                 -> IO [Extremum Prediction]
getModelTimes location begin end = do
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

fmtXtTime     = formatTime defaultTimeLocale "%F %H:%M"
fmtXtInterval = formatTime defaultTimeLocale "%H:%M" . timeToTimeOfDay . realToFrac
readsXtTime   = readsTime  defaultTimeLocale "%F %l:%M %p %Z"
parseXtTime s = case readsXtTime s of
    [x] -> x
    _   -> error $ "Can't parse time: " ++ s
fmtXtType Maximum    = "High"
fmtXtType Minimum    = "Low"
fmtXtType Inflection = "Stationary" -- Never happens?
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
      where -- Period supported by current tide component database
            periodStart = UTCTime (fromGregorian 1700 1 1) 0
            periodEnd   = UTCTime (fromGregorian 2101 1 1) (-1)
    randomR (lo, hi) g = (PredictionInterval begin end step, g''')
      where (begin,   g'  ) = randomR (prBegin lo, prEnd hi) g
            (minutes, g'' ) = randomR (1, 120) g'
            (number,  g''') = randomR (3, 37) g''
            step = realToFrac (minutes * 60 :: Int)
            end  = min (prEnd hi) $ (number * step) `addUTCTime` begin
