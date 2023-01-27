import Control.Exception (bracket)
import Control.Monad (unless)
import Data.Bool (bool)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Environment (withArgs)
import System.Exit
import System.IO.Compat

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
main = bool exitFailure exitSuccess . and =<< traverse test tests

test :: (String, IO (), [String]) -> IO Bool
test (file, prog, args) = do
    putStrLn $ "==== " ++ file ++ " ===="
    actual <- withArgs args $
        captureStdout prog
    expected <- readFile file
    let equal = actual == expected
    unless equal $
        putStrLn actual
    pure equal

captureStdout :: IO a -> IO String
captureStdout action =
  withTemporaryFile $ \h ->
    redirectStdout h $ do
      _ <- action
      hFlush stdout
      hSeek h AbsoluteSeek 0
      hGetContents' h

redirectStdout :: Handle -> IO a -> IO a
redirectStdout h action =
  bracket
    (hFlush stdout *> hDuplicate stdout <* hDuplicateTo h stdout)
    (\saved -> hDuplicateTo saved stdout *> hClose saved)
    (const action)

withTemporaryFile :: (Handle -> IO a) -> IO a
withTemporaryFile inner = do
  tmp <- getTemporaryDirectory
  bracket
    (openBinaryTempFile tmp "wtf-")
    (\(name, h) -> hClose h >> removeFile name)
    (inner . snd)
