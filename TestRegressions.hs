import Data.Monoid
import Text.Printf
import System.Cmd
import System.Exit

instance Monoid ExitCode where
    mempty = ExitSuccess
    mappend ExitSuccess b = b
    mappend a           _ = a

tests =
  [ ("TestTCD.out"         , "TestTCD"         , "Hinkley")
  , ("TideConstituents.out", "TideConstituents", "2014")
  , ("TideAmplitudes.out"  , "TideAmplitudes"  , "Hinkley 2014")
  , ("Tides.out"           , "Tides"           , "Hinkley '1961-05-26 14:29' '1961-05-28 06:38' 01:26")
  ]

main = mapM test tests >>= exitWith . mconcat

test (file, prog, args) = do
    putStrLn $ "==== " ++ file ++ " ===="
    system $ printf "dist/build/%s/%s %s 2>&1 | diff %s -" prog prog args file
