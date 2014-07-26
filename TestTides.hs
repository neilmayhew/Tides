import Time

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

    heights <- getModelHeights location begin end step

    forM_ heights $ \(t, h) ->
        putStrLn $ formatPrediction t h

    times <- getModelTimes location begin end step

    forM_ times $ \(t, h, ty) ->
        putStrLn $ formatPrediction t h ++ printf " %-4s Tide" ty

formatPrediction :: ZonedTime -> Double -> String
formatPrediction t h = printf "%s %9.6f" (formatTime defaultTimeLocale "%F %H:%M %Z" t) h

getModelHeights :: String -> LocalTime -> LocalTime -> NominalDiffTime
                   -> IO [(ZonedTime, Double)]
getModelHeights location begin end step = do
    let cmd = tideCmd location begin end step "m"
    map parseLine <$> run cmd
  where
    parseLine = second read . parseXtTime

getModelTimes :: String -> LocalTime -> LocalTime -> NominalDiffTime
                 -> IO [(ZonedTime, Double, String)]
getModelTimes location begin end step = do
    let cmd = tideCmd location begin end step "p" ++ " | sed '1,/^$/d'"
    map parseLine <$> run cmd
  where
    parseLine s = (t, h, ty)
      where (t, rest) = parseXtTime s
            [ht, _, ty, _] = words rest
            h = read ht

tideCmd location begin end step mode =
    printf "tide -l '%s' -b '%s' -e '%s' -s '%s' -em pSsMm -m %s 2>/dev/null"
        location (fmtXtTime begin) (fmtXtTime end) (fmtXtInterval step) mode :: String

fmtXtTime     = formatTime defaultTimeLocale "%F %H:%M"
fmtXtInterval = formatTime defaultTimeLocale "%H:%M" . timeToTimeOfDay . realToFrac
readsXtTime   = readsTime  defaultTimeLocale "%F %l:%M %p %Z"
parseXtTime s = case readsXtTime s of
    [x] -> x
    _   -> error $ "Can't parse time: " ++ s

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
