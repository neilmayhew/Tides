module Tides where

import TCD
import TCDExtra
import Harmonics
import Analysis
import Time

import Control.Arrow (first)
import Control.Monad (unless, (<=<))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Function
import Data.Time
import Data.Time.Zones

import qualified Data.List.NonEmpty as NE

type Prediction = (ZonedTime, Double)
type Event      = Extremum Prediction

tides :: MonadIO m => String -> LocalTime -> LocalTime -> NominalDiffTime
         -> m ([Prediction], [Extremum Prediction], String, TZ)
tides station begin end step = do

    num <- maybe (error "Cannot find station") pure =<< searchDbsForStation station

    hdr <- getTideDbHeader

    let nConstituents = hdrConstituents hdr
        baseYear      = hdrStartYear    hdr
        indices       = [0..nConstituents-1]

    speeds <- mapM getSpeed indices

    (rn, r) <- readTideRecord num
    unless (rn == num) $ error "Cannot read record"

    --name    <- return . tshName . trHeader $ r
    --country <- getCountry . trCountry $ r
    tz      <- liftIO . loadSystemTZ . dropWhile (== ':') <=< getTZFile . tshTZFile . trHeader $ r
    units   <- getLevelUnits . trLevelUnits $ r

    let beginUTC = localTimeToUTCTZ' tz begin
        endUTC   = localTimeToUTCTZ' tz end
        nextUTC  = step `addUTCTime` beginUTC
        years    = NE.groupBy ((==) `on` yot) [beginUTC, nextUTC .. endUTC]
        yot      = yearOfTime :: UTCTime -> Int

    let tides' times = do
            let startYear = yearOfTime $ NE.head times
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

                heights = NE.map (tide . toHours . timeOfTheYear) times
                ztimes  = NE.map (toZonedTime tz) times

                beginHour = toHours . timeOfTheYear $ NE.head times
                endHour   = toHours . timeOfTheYear $ NE.last times
                hours = takeWhile (< endHour) [beginHour ..] ++ [endHour]
                slots = zip hours (drop 1 hours)
                reversals = filter (\(t0, t1) -> tide' t0 * tide' t1 <= 0) slots
                events = concatMap findEvents reversals
                findEvents = map toTideEvent . extrema series (1/240) -- 15s
                toTideEvent = fmap . first $ toZonedTime tz . yhTimeToUtcTime . YHTime startYear

            return (NE.zip ztimes heights, events)

          where
            d2r d = d * (pi / 180)
            toZonedTime :: TZ -> UTCTime -> ZonedTime
            toZonedTime z t = utcToZonedTime z' t
              where z' = timeZoneForUTCTime z t

    predictions <- mapM tides' years

    return (concatMap (NE.toList . fst) predictions, concatMap snd predictions, units, tz)

-- Prefer the earlier time to the later time when LT is ambiguous
-- This matches the behaviour of xtide
localTimeToUTCTZ' :: TZ -> LocalTime -> UTCTime
localTimeToUTCTZ' tz lt =
  case localTimeToUTCFull tz lt of
    LTUNone ut _ -> ut
    LTUUnique ut _ -> ut
    LTUAmbiguous ut _ _ _ -> ut
