{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

#if IN_TEST_HARNESS
module TideAmplitudes where

import Prelude hiding (IO, print, putStr, putStrLn)
import System.IO.Fake
#endif

import TCD
import TCDExtra

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs, getProgName)
import System.Exit (die)
import Text.Printf

main :: IO ()
main = liftIO getArgs >>= \case

  [station, date] -> do

    num <- maybe (liftIO $ die "Cannot find station") pure =<< searchDbsForStation station

    hdr <- getTideDbHeader

    let nConstituents = fromIntegral (hdrConstituents hdr) :: Int
        baseYear      = fromIntegral (hdrStartYear    hdr) :: Int
        yearNum = read date - baseYear
        indices = [0..nConstituents-1]

    nodeFactors  <- mapM (`getNodeFactor` yearNum) indices

    (rn, r) <- readTideRecord num

    unless (rn == num) $ error "Cannot read record"

    let amplitudes = trAmplitudes  r
        offset     = trDatumOffset r

    putStrLn "  i  Amplitude  Factor     Result"
    putStrLn "---  ---------  ------     ------"
    forM_ (zip3 indices amplitudes nodeFactors) $ \(i, a, f) ->
        when (a /= 0.0) $
            putStrLn $ printf "%3d  %6.4f  *  %6.4f  =  %6.4f" (i :: Int) a f (a * f)

    let maxamp = innerProduct amplitudes nodeFactors
        m2ft = (/ 0.3048)

    putStrLn $ printf "Maximum amplitude: %.6f - %.6f = %.6f = %.6fft"
        (offset+maxamp) (offset-maxamp) (2*maxamp) (m2ft $ 2*maxamp)

  _ -> liftIO $ die . printf "Usage: %s STATION DATE" =<< getProgName

  where
    innerProduct xs ys = sum $ zipWith (*) xs ys
