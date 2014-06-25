import TCD

import Control.Monad
import System.Environment
import System.Exit
import System.IO (hPutStrLn, stderr)
import Text.Printf

main = do
    (station:date:_) <- getArgs

    opened <- openTideDb "/usr/share/xtide/harmonics-dwf-20100529-nonfree.tcd"

    unless opened $ die "Cannot open tide database"

    hdr <- getTideDbHeader

    let nConstituents = fromIntegral (hdrConstituents hdr) :: Int
        baseYear      = fromIntegral (hdrStartYear    hdr) :: Int
        yearNum = read date - baseYear
        indices = [0..nConstituents-1]

    nodeFactors  <- mapM (`getNodeFactor` yearNum) indices

    num <- searchStation station

    unless (num >= 0) $ die "Cannot find station"

    (rn, r) <- readTideRecord num

    unless (rn == num) $ die "Cannot read record"

    let amplitudes = trAmplitudes  r
        offset     = trDatumOffset r

    putStrLn "  i  Amplitude  Factor     Result"
    putStrLn "---  ---------  ------     ------"
    forM_ (zip3 indices amplitudes nodeFactors) $ \(i, a, f) ->
        when (a /= 0.0) $
            putStrLn $ printf "%3d  %6.4f  *  %6.4f  =  %6.4f" (i :: Int) a f (a * f)

    let maxamp = innerProduct amplitudes nodeFactors
        m2ft = (/ 0.3048)

    putStrLn $ "Maximum amplitude: "
        ++ show (offset+maxamp) ++ " - " ++ show (offset-maxamp)
        ++ " = " ++ show (       2*maxamp)
        ++ " = " ++ show (m2ft $ 2*maxamp) ++ "ft"
  where
    die msg = do
        hPutStrLn stderr msg
        exitFailure
    innerProduct xs ys = sum $ zipWith (*) xs ys
