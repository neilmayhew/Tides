import Control.Arrow (first, second)
import Control.Monad
import Data.Functor
import Data.Time
import HSH
import System.Environment
import System.Locale (defaultTimeLocale)
import Text.Printf

main = do
    [location, begin, end, step] <- getArgs

    let toTime     = readTime defaultTimeLocale "%F"
        toInterval = realToFrac . timeOfDayToTime . readTime defaultTimeLocale "%H:%M"

    predictions <- getModel location (toTime begin) (toTime end) (toInterval step)

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
