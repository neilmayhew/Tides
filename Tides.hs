import TCD
import Harmonics
import Analysis
import Time

import Control.Arrow (first)
import Control.Monad
import Data.Function
import Data.List
import Data.Time
import Data.Time.Zones
import System.Environment
import System.Exit
import System.Locale (defaultTimeLocale)
import Text.Printf

main = do
    [location, begin, end, step] <- getArgs

    let toTime     = readTime defaultTimeLocale "%F %H:%M"
        toInterval = realToFrac . timeOfDayToTime . readTime defaultTimeLocale "%H:%M"

    (heights, events, units) <- tides location  (toTime begin) (toTime end) (toInterval step)

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

type Prediction = (ZonedTime, Double)

tides :: String -> LocalTime -> LocalTime -> NominalDiffTime
         -> IO ([Prediction], [Extremum Prediction], String)
tides station begin end step = do

    opened <- openTideDb defaultTideDbPath
    unless opened $ error "Cannot open tide database"

    hdr <- getTideDbHeader

    let nConstituents = hdrConstituents hdr
        baseYear      = hdrStartYear    hdr
        indices       = [0..nConstituents-1]

    speeds <- mapM getSpeed indices

    num <- searchStation station
    unless (num >= 0) $ error "Cannot find station"

    (rn, r) <- readTideRecord num
    unless (rn == num) $ error "Cannot read record"

    name    <- return . tshName . trHeader $ r
    country <- getCountry . trCountry $ r
    tz      <- loadSystemTZ . tail <=< getTZFile . tshTZFile . trHeader $ r
    units   <- getLevelUnits . trLevelUnits $ r

    let beginUTC = localTimeToUTCTZ tz begin
        endUTC   = localTimeToUTCTZ tz end
        nextUTC  = step `addUTCTime` beginUTC
        years    = groupBy ((==) `on` yearOfTime) [beginUTC, nextUTC .. endUTC]

    let tides' times = do
            let startYear = yearOfTime $ head times
                yearNum = fromIntegral startYear - baseYear

            nodeFactors  <- mapM (`getNodeFactor`  yearNum) indices
            equilibriums <- mapM (`getEquilibrium` yearNum) indices

            let amplitudes =           zipWith (*) nodeFactors  (trAmplitudes r)
                phases     = map d2r $ zipWith (-) equilibriums (trEpochs r)
                velocities = map d2r speeds
                offset     = trDatumOffset r

                series  = makeSeries offset amplitudes velocities phases
                series' = differentiate series
                tide    = evaluate series
                tide'   = evaluate series'

                heights = map (tide . toHours . timeOfTheYear) times
                ztimes  = map (toZonedTime tz) times

                beginHour = toHours . timeOfTheYear $ head times
                endHour   = toHours . timeOfTheYear $ last times
                hours = takeWhile (< endHour) [beginHour ..] ++ [endHour]
                slots = zip hours (drop 1 hours)
                reversals = filter (\(t0, t1) -> tide' t0 * tide' t1 <= 0) slots
                events = concatMap findEvents reversals
                findEvents = map toTideEvent . extrema series (1/120)
                toTideEvent = fmap . first $ toZonedTime tz . yhTimeToUtcTime . YHTime startYear

            return (zip ztimes heights, events)

          where
            d2r d = d * (pi / 180)
            toZonedTime :: TZ -> UTCTime -> ZonedTime
            toZonedTime tz t = utcToZonedTime z t
              where z = timeZoneForUTCTime tz t

    predictions <- mapM tides' years

    return (concatMap fst predictions, concatMap snd predictions, units)
