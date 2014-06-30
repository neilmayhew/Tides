module Time where

import Data.Time
import Data.Time.Calendar.OrdinalDate

data YHTime = YHTime
    { yhYear :: Integer
    , yhHour :: Double }
    deriving (Eq, Show, Ord)

utcTimeToYhTime u = YHTime y h
  where b = startOfTheYear y
        h = realToFrac (u `diffUTCTime` b) / 3600
        y = yearOfTime u

yhTimeToUtcTime (YHTime y h) = u
  where b = startOfTheYear y
        u = realToFrac (h * 3600) `addUTCTime` b

yearOfTime :: Num a => UTCTime -> a
yearOfTime = fromInteger . fst . toOrdinalDate . utctDay

timeOfTheYear :: UTCTime -> NominalDiffTime
timeOfTheYear t = t `diffUTCTime` startOfTheYear y
  where y = yearOfTime t

startOfTheYear :: Integer -> UTCTime
startOfTheYear y = UTCTime (fromGregorian y 1 1) 0
