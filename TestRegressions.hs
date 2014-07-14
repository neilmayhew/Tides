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
                 , ("Tides"           , "Hinkley '2014-06-13 00:00' '2014-06-14 00:00' 01:00")
                 ]
        >>= exitWith . mconcat

test (prog, args) = do
    putStrLn $ "==== " ++ prog ++ " ===="
    system $ printf "dist/build/%s/%s %s 2>&1 | diff %s.out -" prog prog args prog
