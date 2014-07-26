import TCD
import TCDExtra

import Control.Monad
import Data.Functor
import Data.List
import Data.Maybe
import System.Environment
import System.Exit
import System.IO (hPutStrLn, stderr)
import Text.Printf

main = do
    (station:_) <- getArgs

    putStr "nullSlackOffset: "
    print nullSlackOffset
    putStr "amplitudeEpsilon: "
    print amplitudeEpsilon

    opened <- openTideDb defaultTideDbPath

    unless opened $ error "Cannot open tide database"

    hdr <- getTideDbHeader
    putStr "getTideDbHeader: "
    print hdr

    putStrLn $ printf "constituents     : %d" $ hdrConstituents    hdr
    putStrLn $ printf "start_year       : %d" $ hdrStartYear       hdr
    putStrLn $ printf "number_of_years  : %d" $ hdrNumberOfYears   hdr
    putStrLn $ printf "number_of_records: %d" $ hdrNumberOfRecords hdr

    num <- searchStation station
    putStr "searchStation: "
    print num

    unless (num >= 0) $ error "Cannot find station"

    (rn, r) <- readTideRecord num
    putStr "readTideRecord: "
    print rn

    unless (rn == num) $ error "Cannot read record"

    putStr =<< formatTideRecord r

    putStrLn "getNearestPartialTideRecord 51.25 (-3.0):"
    print =<< getNearestPartialTideRecord 51.25 (-3.0)

    putStr "getTZFile: "
    putStrLn =<< (getTZFile . tshTZFile . trHeader) r

    putStr "getLevelUnits: "
    putStrLn =<< (getLevelUnits . trLevelUnits) r

    let amplitudes = trAmplitudes r
        cn = fromMaybe 0 $ findIndex (/= 0.0) amplitudes

    putStrLn "getEquilibriums:"
    print =<< (map realToFrac <$> getEquilibriums cn :: IO [Float])
    putStrLn "getNodeFactors:"
    print =<< (map realToFrac <$> getNodeFactors  cn :: IO [Float])
