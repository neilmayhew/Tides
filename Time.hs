module Time where

import Control.Arrow (first, second)
import Data.Time
import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock.POSIX
import System.Random

data YHTime = YHTime
    { yhYear :: Integer
    , yhHour :: Double }
    deriving (Eq, Show, Ord)

utcTimeToYhTime u = YHTime y h
  where b = startOfTheYear y
        h = toHours $ u `diffUTCTime` b
        y = yearOfTime u

yhTimeToUtcTime (YHTime y h) = u
  where b = startOfTheYear y
        u = fromHours h `addUTCTime` b

toHours :: NominalDiffTime -> Double
toHours d = realToFrac d / 3600

fromHours :: Double -> NominalDiffTime
fromHours h = realToFrac (h * 3600)

yearOfTime :: Num a => UTCTime -> a
yearOfTime = fromInteger . fst . toOrdinalDate . utctDay

timeOfTheYear :: UTCTime -> NominalDiffTime
timeOfTheYear t = t `diffUTCTime` startOfTheYear y
  where y = yearOfTime t

startOfTheYear :: Integer -> UTCTime
startOfTheYear y = UTCTime (fromGregorian y 1 1) 0

instance Enum UTCTime where
    succ                   = addUTCTime ( 1)
    pred                   = addUTCTime (-1)
    toEnum                 = posixSecondsToUTCTime . fromIntegral
    fromEnum               = round . utcTimeToPOSIXSeconds

    enumFrom x             = iterate succ x
    enumFromThen x y       = iterate (addUTCTime d) x where d = y `diffUTCTime` x
    enumFromTo x y         = takeWhile (<= y) $ enumFrom x
    enumFromThenTo x1 x2 y = takeWhile (<= y) $ enumFromThen x1 x2

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
