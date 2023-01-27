import Control.Exception (bracket)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Process
import System.Environment (withArgs)
import System.Exit
import System.IO
import Text.Printf

import qualified TestTCD
import qualified TideConstituents
import qualified TideAmplitudes
import qualified TidesMain

tests :: [(String, IO (), [String])]
tests =
  [ ("TestTCD.out"         , TestTCD.main         , ["Hinkley"])
  , ("TideConstituents.out", TideConstituents.main, ["2014"])
  , ("TideAmplitudes.out"  , TideAmplitudes.main  , ["Hinkley", "2014"])
  , ("Tides.out"           , TidesMain.main       , ["Hinkley", "1961-05-26 14:29", "1961-05-28 06:38", "01:26"])
  , ("Tides-DST-begin.out" , TidesMain.main       , ["Hinkley", "1960-04-09 23:00", "1960-04-10 07:00", "00:20"])
  , ("Tides-DST-end.out"   , TidesMain.main       , ["Hinkley", "1960-10-01 22:00", "1960-10-02 05:00", "00:20"])
  , ("Tides-YearEnd.out"   , TidesMain.main       , ["Hinkley", "2013-12-31 22:00", "2014-01-01 02:00", "00:05"])
  ]

main :: IO ()
main = exitWith . maximum =<< traverse test tests

test :: (String, IO (), [String]) -> IO ExitCode
test (file, prog, args) = do
    putStrLn $ "==== " ++ file ++ " ===="
    let actual = "actual"
    withArgs args $
      withFile actual WriteMode $
        redirectStdout prog
    system $ printf "diff %s %s" file actual

redirectStdout :: IO a -> Handle -> IO a
redirectStdout action h =
  bracket
    (hFlush stdout *> hDuplicate stdout <* hDuplicateTo h stdout)
    (\saved -> hDuplicateTo saved stdout *> hClose saved)
    (const action)
