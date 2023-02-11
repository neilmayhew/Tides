import Data.Foldable (for_)
import System.Environment (withArgs)
import Test.Hspec
import Test.Hspec.Golden

import qualified TestTCD
import qualified TideConstituents
import qualified TideAmplitudes
import qualified TidesMain

import qualified System.IO.Fake as Fake

tests :: [(String, Fake.IO (), [String])]
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
    for_ tests $ \(file, prog, args) -> do
      before (withArgs args $ Fake.execIO prog) $
        specify file $
          defaultGolden file
