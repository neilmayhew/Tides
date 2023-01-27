{-# LANGUAGE CPP #-}

#if IN_TEST_HARNESS
module TestTCD where
#endif

import TCD
import TCDExtra

import Control.Monad
import Data.List (findIndex)
import Data.Maybe
import System.Environment
import System.Exit
import Text.Printf

main :: IO ()
main = do
    (station:_) <- getArgs

    putStr "nullSlackOffset: "
    print nullSlackOffset
    putStr "amplitudeEpsilon: "
    print amplitudeEpsilon

    num <- maybe (die "Cannot find station") pure =<< searchDbsForStation station

    hdr <- getTideDbHeader
    putStr "getTideDbHeader: "
    print hdr

    putStrLn $ printf "constituents     : %d" $ hdrConstituents    hdr
    putStrLn $ printf "start_year       : %d" $ hdrStartYear       hdr
    putStrLn $ printf "number_of_years  : %d" $ hdrNumberOfYears   hdr
    putStrLn $ printf "number_of_records: %d" $ hdrNumberOfRecords hdr

    putStr "searchStation: "
    print num
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
