import TCD
import Harmonics
import Analysis

import Control.Monad
import Data.Time
import Data.Time.Calendar.OrdinalDate
import Data.Time.Zones
import System.Environment
import System.Exit
import System.Locale (defaultTimeLocale)
import Text.Printf

main = do
    (station:date:_) <- getArgs

    let start = readTime defaultTimeLocale "%F" date
        step = 3600
        duration = step * 24
        localTimes = map (utcToLocalTime utc . (`addUTCTime` start)) [0, step .. duration]

    (times, heights, units, extrema, tz) <- tides station localTimes

    -- Output tide heights for the period

    forM_ (zip times heights) $ \(t, h) ->
        putStrLn $ printf "%s %10.6f" (showTime tz t) h

    -- Output low and high tides for the period

    forM_ extrema $ \(Extremum t h c) ->
        putStrLn $ printf "%s %6.2f %s  %s Tide"
            (showTime tz t) h units (showType c)
  where
    showTime :: TZ -> UTCTime -> String
    showTime tz t = formatTime defaultTimeLocale "%F %l:%M %p %Z" zt
      where t' = 30 `addUTCTime` t -- Round to nearest minute
            z = timeZoneForUTCTime tz t'
            zt = utcToZonedTime z t'
    showType Maximum    = "High"
    showType Minimum    = "Low"
    showType Inflection = "Stationary" -- Never happens?

tides :: String -> [LocalTime]
         -> IO ([UTCTime], [Double], String, [Extremum UTCTime Double], TZ)
tides station localTimes = do

    opened <- openTideDb "/usr/share/xtide/harmonics-dwf-20100529-nonfree.tcd"
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

    name <- return . tshName . trHeader $ r
    country <- getCountry . trCountry $ r
    putStrLn $ printf "Station: %s, %s" name country

    tzname <- liftM tail . getTZFile . tshTZFile . trHeader $ r
    tz <- loadSystemTZ tzname

    units <- getLevelUnits . trLevelUnits $ r

    -- TODO: handle crossing year boundaries

    let times = map (localTimeToUTCTZ tz) localTimes
        startUTC = head times
        yearUTC = startOfTheYear startUTC
        utc2hr t = realToFrac (t `diffUTCTime` yearUTC) / 3600
        hr2utc h = realToFrac (h * 3600) `addUTCTime` yearUTC
        yearNum = yearOfTime startUTC - baseYear

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

        hours = map utc2hr times
        heights = map tide hours

        slots = zip hours (map (+1) hours)
        reversals = filter (\(t0, t1) -> tide' t0 * tide' t1 <= 0) slots
        events = concatMap findEvents reversals
        findEvents = map toTideEvent . extrema series (1/120)
        toTideEvent (Extremum t h c) = Extremum (hr2utc t) h c

    return (times, heights, units, events, tz)

  where
    d2r d = d * (pi / 180)

yearOfTime :: Num a => UTCTime -> a
yearOfTime = fromInteger . fst . toOrdinalDate . utctDay

timeOfTheYear :: UTCTime -> NominalDiffTime
timeOfTheYear t = t `diffUTCTime` startOfTheYear t

startOfTheYear :: UTCTime -> UTCTime
startOfTheYear t = UTCTime (fromGregorian year 1 1) 0
  where year = yearOfTime t
