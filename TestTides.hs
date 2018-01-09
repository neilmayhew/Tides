{-# LANGUAGE CPP #-}

module Main where

import Tides
import Time
import Analysis

import Control.Arrow (first, second)
import Control.Monad
import Data.Bool (bool)
import Data.Functor ((<$>))
import Data.Function (on)
import Data.Time
import Data.Time.Locale.Compat (defaultTimeLocale)
import Data.Time.Zones (TZ, LocalToUTCResult(..), localTimeToUTCFull)
import HSH (run)
import System.Environment
import System.Exit
import Test.QuickCheck
import Text.Printf
import Test.QuickCheck.Monadic as QCM (assert, monadicIO, run)

#if !MIN_VERSION_time(1,5,0)
import Data.Time.Locale.Compat (TimeLocale)
parseTimeOrError :: ParseTime t => Bool -> TimeLocale -> String -> String -> t
parseTimeOrError _ = readTime
readSTime :: ParseTime t => Bool -> TimeLocale -> String -> ReadS t
readSTime _ = readsTime
#endif

main :: IO ()
main = do
    (location:args) <- getArgs

    interval <- generate arbitrary

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

    predictionMismatches <- comparePredictions location begin end step
    eventMismatches      <- compareEvents      location begin end step

    unless (null predictionMismatches) $
        putStrLn "Predictions don't match"

    forM_ predictionMismatches $ \(a, b) ->
        putStrLn $ formatPrediction a ++ " / " ++ formatPrediction b

    unless (null eventMismatches) $
        putStrLn "Events don't match"

    forM_ eventMismatches $ \(a, b) ->
        putStrLn $ formatEvent a ++ " / " ++ formatEvent b

    bool exitFailure exitSuccess $ null predictionMismatches && null eventMismatches

comparePredictions :: String -> LocalTime -> LocalTime -> NominalDiffTime -> IO [(Prediction, Prediction)]
comparePredictions location begin end step = do
    (predictions, _, _, tz) <- tides                  location begin end step
    modelPredictions        <- getModelPredictions tz location begin end step

    let predictionPairs = zip modelPredictions predictions

        eqPred  (t, h) (t', h') = eqTime t t' && abs (h - h') < 1e-6
        eqTime t t' = ztzName t == ztzName t' && abs (t `diffZonedTime` t') < 60
          where ztzName = timeZoneName . zonedTimeZone

    return $ filter (not . uncurry eqPred ) predictionPairs

prop_equalPredictions :: String -> PredictionInterval -> Property
prop_equalPredictions location interval = monadicIO $ do
    let zone = utc -- TODO: get from location
        toLocal = clampTime . utcToLocalTime zone
        clampTime (LocalTime d t) = LocalTime d t { todSec = 0 }
        (begin, end, step) = (toLocal $ prBegin interval, toLocal $ prEnd interval, prStep interval)
    predictionMismatches <- QCM.run $ comparePredictions location begin end step
    assert $ null predictionMismatches

compareEvents :: String -> LocalTime -> LocalTime -> NominalDiffTime -> IO [(Event, Event)]
compareEvents location begin end step = do
    (_, events, _, tz) <- tides                  location begin end step
    modelEvents        <- getModelEvents      tz location begin end

    let eventPairs = zip modelEvents events

        eqEvent (Extremum (t, h) c) (Extremum (t', h') c') = (c == c') && eqTime t t' && abs (h - h') < 0.006
        eqTime t t' = ztzName t == ztzName t' && abs (t `diffZonedTime` t') <= 90
          where ztzName = timeZoneName . zonedTimeZone

    return $ filter (not . uncurry eqEvent) eventPairs

prop_equalEvents :: String -> PredictionInterval -> Property
prop_equalEvents location interval = monadicIO $ do
    let zone = utc -- TODO: get from location
        toLocal = clampTime . utcToLocalTime zone
        clampTime (LocalTime d t) = LocalTime d t { todSec = 0 }
        (begin, end, step) = (toLocal $ prBegin interval, toLocal $ prEnd interval, prStep interval)
    eventMismatches <- QCM.run $ compareEvents location begin end step
    assert $ null eventMismatches

formatPrediction :: Prediction -> String
formatPrediction (t, h) = printf "%s %9.6f" (formatTime defaultTimeLocale "%F %H:%M:%S %Z" t) h
formatEvent :: Event -> String
formatEvent (Extremum p c) = formatPrediction p ++ printf " %-4s Tide" (fmtXtType c)

getModelPredictions :: TZ -> String -> LocalTime -> LocalTime -> NominalDiffTime -> IO [Prediction]
getModelPredictions tz location begin end step = do
    let cmd = tideCmd location begin end (Just step) "m"
    map parseLine <$> HSH.run cmd
  where
    parseLine = second read . parseXtTime tz

getModelEvents :: TZ -> String -> LocalTime -> LocalTime -> IO [Event]
getModelEvents tz location begin end = do
    let cmd = tideCmd location begin end Nothing "p" ++ " | sed '1,/^$/d'"
    map parseLine <$> HSH.run cmd
  where
    parseLine s = Extremum (t, h) c
      where (t, rest) = parseXtTime tz s
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
parseXtTime   :: TZ -> String -> (ZonedTime, String)
parseXtTime tz s = case readsXtTime s of
    [x] -> first reifyZonedTime' x
    _   -> error $ "Can't parse time: " ++ s
  where
    eitherToError = either error id
    reifyZonedTime' = eitherToError . reifyZonedTime tz

fmtXtType :: Criticality -> String
fmtXtType Maximum    = "High"
fmtXtType Minimum    = "Low"
fmtXtType Inflection = "Stationary" -- Never happens?

parseXtType :: String -> Criticality
parseXtType t = case t of
    "Low" -> Minimum
    "High" -> Maximum
    _ -> error $ "Unknown tide type: " ++ t

-- Data.Time's %Z parsing doesn't create a real TimeZone, just a fake UTC
-- one with the parsed abbreviation as its name. However, knowing the TZ
-- does allow a valid local time with abbreviation to be converted into
-- a real ZonedTime.
reifyZonedTime :: TZ -> ZonedTime -> Either String ZonedTime
reifyZonedTime tz zt@(ZonedTime t z) =
    let zn = timeZoneName z
    in case localTimeToUTCFull tz t of
        LTUUnique      ut   z'@(TimeZone _ _ zn')   | zn' == zn -> Right $ utcToZonedTime z' ut
        LTUAmbiguous   ut _ z'@(TimeZone _ _ zn') _ | zn' == zn -> Right $ utcToZonedTime z' ut
        LTUAmbiguous _ ut _ z'@(TimeZone _ _ zn')   | zn' == zn -> Right $ utcToZonedTime z' ut
        LTUNone _ _ -> Left $ "Non-existent local time: " ++ show zt
        _           -> Left $ "Inappropriate zone abbreviation: " ++ show zt

addZonedTime :: NominalDiffTime -> ZonedTime -> ZonedTime
addZonedTime ndt zt = utcToZonedTime' . addUTCTime ndt . zonedTimeToUTC $ zt
  where utcToZonedTime' = utcToZonedTime (zonedTimeZone zt)

diffZonedTime :: ZonedTime -> ZonedTime -> NominalDiffTime
diffZonedTime = diffUTCTime `on` zonedTimeToUTC

data PredictionInterval = PredictionInterval
    { prBegin :: UTCTime
    , prEnd   :: UTCTime
    , prStep  :: NominalDiffTime
    } deriving (Eq, Show)

instance Arbitrary PredictionInterval where

    arbitrary = sized $ \number -> do
        minutes <- choose (1, 120 :: Int)
        begin <- choose (periodStart, periodEnd)
        let step = realToFrac (minutes * 60)
            end  = min periodEnd $ (realToFrac number * step) `addUTCTime` begin
        return $ PredictionInterval begin end step
      where -- Period supported by current tide component database is 1700-2101
            -- However, date code seems to have problems outside 1848-2037
        periodStart = UTCTime (fromGregorian 1848 1 1) 0
        periodEnd   = UTCTime (fromGregorian 2037 1 1) (-1)

    shrink (PredictionInterval b e s) =
        if e `diffUTCTime` b > s
            then map single [b, b `plus` s .. e `minus` s]
            else []
      where
        single t = PredictionInterval t (t `plus` s) s
        plus  t d = addUTCTime d t
        minus t d = addUTCTime (negate d) t
