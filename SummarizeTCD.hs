import TCD

import Control.Monad
import Data.Set
import System.Environment
import Text.Printf

main :: IO ()
main = do
    (db:_) <- getArgs

    opened <- openTideDb db

    unless opened $ error "Cannot open tide database"

    hdr <- getTideDbHeader

    putStrLn $ printf "constituents     : %d" $ hdrConstituents    hdr
    putStrLn $ printf "start_year       : %d" $ hdrStartYear       hdr
    putStrLn $ printf "number_of_years  : %d" $ hdrNumberOfYears   hdr
    putStrLn $ printf "number_of_records: %d" $ hdrNumberOfRecords hdr

    let accumulate (nsub, latMin, latMax, lonMin, lonMax, zones) i = do

            (ok, r) <- getPartialTideRecord i

            unless ok $ error "Cannot read record"

            return ( nsub + fromEnum (tshType r)
                   , latMin `min` tshLatitude r
                   , latMax `max` tshLatitude r
                   , lonMin `min` tshLongitude r
                   , lonMax `max` tshLongitude r
                   , insert (tshTZFile r) zones )

        n = hdrNumberOfRecords hdr

    (nsub, latMin, latMax, lonMin, lonMax, zones)
        <- foldM accumulate (0, 180, -180, 180, -180, empty) [0 .. n-1]

    putStrLn $ printf "%d stations (%d reference, %d subordinate)" n (n - nsub) nsub
    putStrLn $ printf "%f-%f latitude"  latMin latMax
    putStrLn $ printf "%f-%f longitude" lonMin lonMax
    putStrLn $ printf "%d timezones" (size zones)

    forM_ [0..n-1] $ \i -> do

        (ok, r) <- getPartialTideRecord i

        unless ok $ error "Cannot read record"

        putStrLn $ tshName r
