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
    [location] <- getArgs

    let zone = utc -- TODO: get from location

    interval <- randomIO

    let begin = toLocal $ prBegin interval
        end   = toLocal $ prEnd   interval
        step  =           prStep  interval
        toLocal = utcToLocalTime zone

    predictions <- getModel location begin end step

    forM_ predictions $ putStrLn . formatPrediction

formatPrediction :: (ZonedTime, Double) -> String
formatPrediction (t, h) = printf "%s %.6f" (formatTime defaultTimeLocale "%F %H:%M %Z" t) h

getModel :: String -> LocalTime -> LocalTime -> NominalDiffTime
            -> IO [(ZonedTime, Double)]
getModel location begin end step =
    let cmd = printf "tide -l '%s' -b '%s' -e '%s' -s '%s' -m m -em pSsMm 2>/dev/null"
                location (xtideTime begin) (xtideTime end) (xtideInterval step) :: String
    in map parseLine <$> run cmd
  where
    xtideTime     = formatTime defaultTimeLocale "%F %H:%M"
    xtideInterval = formatTime defaultTimeLocale "%H:%M" . timeToTimeOfDay . realToFrac
    parseLine = second read . head . readsTime defaultTimeLocale "%F %l:%M %p %Z"

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

instance Random DiffTime where
    random = first secondsToDiffTime . random
    randomR (min, max) = first secondsToDiffTime . randomR (round min, round max)

instance Random NominalDiffTime where
    random = first timeToNominal . random
    randomR (min, max) = first timeToNominal . randomR (nominalToTime min, nominalToTime max)

instance Random TimeOfDay where
    random = first timeToTimeOfDay . randomR (0, 86399)
    randomR (min, max) = first timeToTimeOfDay . randomR (timeOfDayToTime min, timeOfDayToTime max)

instance Random Day where
    random = first ModifiedJulianDay . random
    randomR (min, max) = first ModifiedJulianDay . randomR (toModifiedJulianDay min, toModifiedJulianDay max)

instance Random LocalTime where
    randomR (min, max) = first (utcToLocalTime utc) . randomR (localTimeToUTC utc min, localTimeToUTC utc max)
    random = first (utcToLocalTime utc) . random

instance Random UTCTime where
    randomR (min, max) = first (`addUTCTime` min) . randomR (0, max `diffUTCTime` min)
    random g = (UTCTime d t, g'')
      where (d, g' ) = random g
            (t, g'') = first timeOfDayToTime $ random g'

timeToNominal = realToFrac :: DiffTime -> NominalDiffTime
nominalToTime = realToFrac :: NominalDiffTime -> DiffTime
