import TCD

import Control.Monad (unless)
import Data.List (zip5)
import System.Environment (getArgs)
import System.Exit
import System.IO (hPutStrLn, stderr)
import Text.Printf

main = do
    (date:_) <- getArgs

    opened <- openTideDb "/usr/share/xtide/harmonics-dwf-20100529-nonfree.tcd"

    unless opened $ die "Cannot open tide database"

    hdr <- getTideDbHeader

    let nConstituents = fromIntegral (hdrConstituents hdr) :: Int
        baseYear      = fromIntegral (hdrStartYear    hdr) :: Int
        yearNum = read date - baseYear
        indices = [0..nConstituents-1]

    names        <- mapM getConstituent indices
    speeds       <- mapM getSpeed       indices
    nodeFactors  <- mapM (`getNodeFactor`  yearNum) indices
    equilibriums <- mapM (`getEquilibrium` yearNum) indices

    putStrLn "  i Name             Speed Factor Equilibrium"
    putStrLn "--- ---------- ----------- ------ -----------"
    putStr . unlines $ map
        (\(i, n, s, f, e) -> printf "%3d %-10s %11.7f %6.4f %6.2f" i n s f e)
        (zip5 indices names speeds nodeFactors equilibriums)
  where
    die msg = do
        hPutStrLn stderr msg
        exitFailure
