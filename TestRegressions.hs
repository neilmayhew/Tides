import Data.Monoid
import Text.Printf
import System.Cmd
import System.Exit

instance Monoid ExitCode where
    mempty = ExitSuccess
    mappend ExitSuccess b = b
    mappend a           _ = a

main = mapM test [ ("TestTCD"         , "Hinkley")
                 , ("TideConstituents", "2014")
                 , ("TideAmplitudes"  , "Hinkley 2014")
                 , ("Tides"           , "Hinkley '1961-05-26 14:29' '1961-05-28 06:38' 01:26")
                 ]
        >>= exitWith . mconcat

test (prog, args) = do
    putStrLn $ "==== " ++ prog ++ " ===="
    system $ printf "dist/build/%s/%s %s 2>&1 | diff %s.out -" prog prog args prog
