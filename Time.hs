{-# OPTIONS_GHC -fno-warn-orphans #-}

module Time where

import Control.Arrow (first)
import Data.Time
import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock.POSIX
import System.Random

data YHTime = YHTime
    { yhYear :: Integer
    , yhHour :: Double }
    deriving (Eq, Show, Ord)

utcTimeToYhTime :: UTCTime -> YHTime
utcTimeToYhTime u = YHTime y h
  where b = startOfTheYear y
        h = toHours $ u `diffUTCTime` b
        y = yearOfTime u

yhTimeToUtcTime :: YHTime -> UTCTime
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
    randomR (from, to) = first secondsToDiffTime . randomR (round from, round to)

instance Random NominalDiffTime where
    random = first timeToNominal . random
    randomR (from, to) = first timeToNominal . randomR (nominalToTime from, nominalToTime to)

instance Random TimeOfDay where
    random = first timeToTimeOfDay . randomR (0, 86399)
    randomR (from, to) = first timeToTimeOfDay . randomR (timeOfDayToTime from, timeOfDayToTime to)

instance Random Day where
    random = first ModifiedJulianDay . random
    randomR (from, to) = first ModifiedJulianDay . randomR (toModifiedJulianDay from, toModifiedJulianDay to)

instance Random LocalTime where
    randomR (from, to) = first (utcToLocalTime utc) . randomR (localTimeToUTC utc from, localTimeToUTC utc to)
    random = first (utcToLocalTime utc) . random

instance Random UTCTime where
    randomR (from, to) = first (`addUTCTime` from) . randomR (0, to `diffUTCTime` from)
    random g = (UTCTime d t, g'')
      where (d, g' ) = random g
            (t, g'') = first timeOfDayToTime $ random g'

timeToNominal :: DiffTime -> NominalDiffTime
timeToNominal = realToFrac
nominalToTime :: NominalDiffTime -> DiffTime
nominalToTime = realToFrac
