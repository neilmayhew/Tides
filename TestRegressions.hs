import Control.Exception (bracket)
import Data.Foldable (traverse_)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Environment (withArgs)
import System.IO.Compat
import Test.Hspec
import Test.Hspec.Golden

import qualified TestTCD
import qualified TideConstituents
import qualified TideAmplitudes
import qualified TidesMain

tests :: [(String, IO (), [String])]
tests =
  [ ("TestTCD"         , TestTCD.main         , ["Hinkley"])
  , ("TideConstituents", TideConstituents.main, ["2014"])
  , ("TideAmplitudes"  , TideAmplitudes.main  , ["Hinkley", "2014"])
  , ("Tides"           , TidesMain.main       , ["Hinkley", "1961-05-26 14:29", "1961-05-28 06:38", "01:26"])
  , ("Tides-DST-begin" , TidesMain.main       , ["Hinkley", "1960-04-09 23:00", "1960-04-10 07:00", "00:20"])
  , ("Tides-DST-end"   , TidesMain.main       , ["Hinkley", "1960-10-01 22:00", "1960-10-02 05:00", "00:20"])
  , ("Tides-YearEnd"   , TidesMain.main       , ["Hinkley", "2013-12-31 22:00", "2014-01-01 02:00", "00:05"])
  ]

main :: IO ()
main = hspec $
  describe "Golden Tests" $
    traverse_ goldenTest tests

goldenTest :: (String, IO (), [String]) -> Spec
goldenTest (file, prog, args) = do
  before (withArgs args $ captureStdout prog) $
    specify file $
      defaultGolden file

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
