import TCD

import Control.Exception (catch, SomeException(..))
import Control.Monad (forM_, unless, when)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

main :: IO ()
main = do
    (station:dbs) <- getArgs

    forM_ dbs $ \db -> do
        catch (searchDb station db)
              (\e -> hPutStrLn stderr $ printf "%s: %s" db $ show (e :: SomeException))

searchDb :: String -> FilePath -> IO ()
searchDb station db = do

    opened <- openTideDb db

    unless opened $ fail "Cannot open tide database"

    count <- hdrNumberOfRecords <$> getTideDbHeader

    let loop = do
        num <- searchStation station

        when (num >= 0) $ do
            (ok, r) <- getPartialTideRecord num

            unless ok $ fail "Cannot read record"

            putStrLn $ printf "%s\t%s\t%d/%d" db (tshName r) num count

            loop

    loop

    closeTideDb
