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

    opened <- openTideDb "/usr/share/xtide/harmonics-dwf-20100529-nonfree.tcd"

    unless opened $ die "Cannot open tide database"

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

    unless (num >= 0) $ die "Cannot find station"

    (rn, r) <- readTideRecord num
    putStr "readTideRecord: "
    print rn

    unless (rn == num) $ die "Cannot read record"

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
    print =<< getEquilibriums cn
    putStrLn "getNodeFactors:"
    print =<< getNodeFactors  cn
  where
    die msg = do
        hPutStrLn stderr msg
        exitFailure
