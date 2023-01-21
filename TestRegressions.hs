import System.Process
import System.Exit
import System.IO
import Text.Printf

tests :: [(String, String, String)]
tests =
  [ ("TestTCD.out"         , "TestTCD"         , "Hinkley")
  , ("TideConstituents.out", "TideConstituents", "2014")
  , ("TideAmplitudes.out"  , "TideAmplitudes"  , "Hinkley 2014")
  , ("Tides.out"           , "Tides"           , "Hinkley '1961-05-26 14:29' '1961-05-28 06:38' 01:26")
  , ("Tides-DST-begin.out" , "Tides"           , "Hinkley '1960-04-09 23:00' '1960-04-10 07:00' 00:20")
  , ("Tides-DST-end.out"   , "Tides"           , "Hinkley '1960-10-01 22:00' '1960-10-02 05:00' 00:20")
  , ("Tides-YearEnd.out"   , "Tides"           , "Hinkley '2013-12-31 22:00' '2014-01-01 02:00' 00:05")
  ]

main :: IO ()
main = exitWith . maximum =<< traverse test tests

test :: (String, String, String) -> IO ExitCode
test (file, prog, args) = do
    putStrLn $ "==== " ++ file ++ " ===="
    hFlush stdout
    system $ printf "dist/build/%s/%s %s 2>&1 | diff %s -" prog prog args file
