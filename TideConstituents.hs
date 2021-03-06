import TCD
import TCDExtra

import Control.Monad (forM_, unless)
import Data.List (sort, zip5)
import System.Environment (getArgs)
import Text.Printf

main :: IO ()
main = do
    (date:_) <- getArgs

    opened <- openDefaultTideDb

    unless opened $ error "Cannot open tide database"

    hdr <- getTideDbHeader

    let nConstituents = fromIntegral (hdrConstituents hdr) :: Int
        baseYear      = fromIntegral (hdrStartYear    hdr) :: Int
        yearNum = read date - baseYear
        indices = [0..nConstituents-1]

    names        <- mapM getConstituent indices
    speeds       <- mapM getSpeed       indices
    nodeFactors  <- mapM (`getNodeFactor`  yearNum) indices
    equilibriums <- mapM (`getEquilibrium` yearNum) indices

    let constituents = zip5 speeds indices names nodeFactors equilibriums

    putStrLn "  i Name             Speed Factor Equilib       Period"
    putStrLn "--- ---------- ----------- ------ ------- ------------"
    forM_ (sort constituents) $
        \(s, i, n, f, e) -> do
            let p = showPeriod $ 360 / s
            printf "%3d %-10s %11.7f %6.4f %7.2f %12s\n" i n s f e p

showPeriod :: Double -> String
showPeriod p =
    let (h', m) = properFraction p :: (Int, Double)
        (d , h) = quotRem h' 24
    in concat
        [ if d > 0 then printf "%d:%02d" d h else printf "%d" h
        , printf ":%05.2f" (m*60) ]
