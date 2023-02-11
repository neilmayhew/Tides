{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

#if IN_TEST_HARNESS
module TideConstituents where

import Prelude hiding (IO, print, putStr, putStrLn)
import System.IO.Fake
#endif

import TCD
import TCDExtra

import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Data.List (sort, zip5)
import System.Environment (getArgs, getProgName)
import System.Exit
import Text.Printf

main :: IO ()
main = liftIO getArgs >>= \case
  [date] -> showConstituents =<< getConstituents date
  _ -> liftIO $ die . printf "Usage: %s DATE" =<< getProgName

getConstituents :: String -> IO [(Double, Int, String, Double, Double)]
getConstituents date = do

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

    pure $ zip5 speeds indices names nodeFactors equilibriums

showConstituents :: [(Double, Int, String, Double, Double)] -> IO ()
showConstituents constituents = do
    putStrLn "  i Name             Speed Factor Equilib       Period"
    putStrLn "--- ---------- ----------- ------ ------- ------------"
    forM_ (sort constituents) $
        \(s, i, n, f, e) -> do
            let p = showPeriod $ 360 / s
            putStrLn $ printf "%3d %-10s %11.7f %6.4f %7.2f %12s" i n s f e p

showPeriod :: Double -> String
showPeriod p =
    let (h', m) = properFraction p :: (Int, Double)
        (d , h) = quotRem h' 24
    in concat
        [ if d > 0 then printf "%d:%02d" d h else printf "%d" h
        , printf ":%05.2f" (m*60) ]
