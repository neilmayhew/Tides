import TCD
import Harmonics
import Analysis
import Time

import Control.Monad
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

    (times, heights, units, extrema, tz) <- tides location  (toTime begin) (toTime end) (toInterval step)

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

tides :: String -> LocalTime -> LocalTime -> NominalDiffTime
         -> IO ([UTCTime], [Double], String, [Extremum UTCTime Double], TZ)
tides station begin end step = do

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

    name    <- return . tshName . trHeader $ r
    country <- getCountry . trCountry $ r
    tz      <- loadSystemTZ . tail <=< getTZFile . tshTZFile . trHeader $ r
    units   <- getLevelUnits . trLevelUnits $ r

    -- TODO: handle crossing year boundaries

    let beginUTC = localTimeToUTCTZ tz begin
        endUTC   = localTimeToUTCTZ tz end
        duration = endUTC `diffUTCTime` beginUTC
        times = map (`addUTCTime` beginUTC) [0, step .. duration]
        yhTimes = map utcTimeToYhTime times
        startYear = yhYear (head yhTimes)
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

        heights = map (tide . yhHour) yhTimes

        beginHour = yhHour (head yhTimes)
        endHour   = yhHour (last yhTimes)
        hours = takeWhile (< endHour) [beginHour ..] ++ [endHour]
        slots = zip hours (drop 1 hours)
        reversals = filter (\(t0, t1) -> tide' t0 * tide' t1 <= 0) slots
        events = concatMap findEvents reversals
        findEvents = map toTideEvent . extrema series (1/120)
        toTideEvent (Extremum t h c) = Extremum (yhTimeToUtcTime $ YHTime startYear t) h c

    return (times, heights, units, events, tz)

  where
    d2r d = d * (pi / 180)
