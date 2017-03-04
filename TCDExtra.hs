{-# LANGUAGE CPP #-}

module TCDExtra where

import TCD
import Data.Functor ((<$>))
import Data.List
import System.Directory
import System.FilePath
import Text.Printf

-- This is designed to match libtcd's dump_tide_record as closely as possible

formatTideRecord :: TideRecord -> IO String
formatTideRecord r =
    unlines . (++ concatMap showConstituent constituents) <$> mapM formatField fields
  where
    formatField :: (String, TideRecord -> IO String) -> IO String
    formatField (n, f) = printf "%s = %s" n <$> f r
    fields =
        [ ("Record number"        , return . show  . tshNumber           . trHeader)
        , ("Record size"          , return . show  . tshSize             . trHeader)
        , ("Record type"          , return . show  . tshType             . trHeader)
        , ("Latitude"             , return . showF . tshLatitude         . trHeader)
        , ("Longitude"            , return . showF . tshLongitude        . trHeader)
        , ("Reference station"    , return . show  . tshReferenceStation . trHeader)
        , ("Tzfile"               , getTZFile      . tshTZFile           . trHeader)
        , ("Name"                 , return         . tshName             . trHeader)
        , ("Country"              , getCountry     . trCountry                     )
        , ("Source"               , return         . trSource                      )
        , ("Restriction"          , getRestriction . trRestriction                 )
        , ("Comments"             , return         . trComments                    )
        , ("Notes"                , return         . trNotes                       )
        , ("Legalese"             , getLegalese    . trLegalese                    )
        , ("Station ID context"   , return         . trStationIdContext            )
        , ("Station ID"           , return         . trStationId                   )
        , ("Date imported"        , return . show  . trDateImported                )
        , ("Xfields"              , return         . trXfields                     )
        , ("Direction units"      , getDirUnits    . trDirectionUnits              )
        , ("Min direction"        , return . show  . trMinDirection                )
        , ("Max direction"        , return . show  . trMaxDirection                )
        , ("Level units"          , getLevelUnits  . trLevelUnits                  )
        , ("Datum offset"         , return . showF . trDatumOffset                 )
        , ("Datum"                , getDatum       . trDatum                       )
        , ("Zone offset"          , return . show  . trZoneOffset                  )
        , ("Expiration date"      , return . show  . trExpirationDate              )
        , ("Months on station"    , return . show  . trMonthsOnStation             )
        , ("Last date on station" , return . show  . trLastDateOnStation           )
        , ("Confidence"           , return . show  . trConfidence                  )
        ]
    showConstituent :: (Int, Double, Double) -> [String]
    showConstituent (i, amp, epoch) =
        if amp /= 0.0
        then [ printf "Amplitude[%d] = %.6f" i amp
             , printf "Epoch[%d] = %.6f"     i epoch]
        else []
    constituents = zip3 [0..] (trAmplitudes r) (trEpochs r)
    showF x = printf "%.6f" x :: String

#ifndef DEFAULT_TIDE_DB_PATH
#define DEFAULT_TIDE_DB_PATH "/usr/share/xtide"
#endif

defaultTideDbPath :: String
defaultTideDbPath = DEFAULT_TIDE_DB_PATH

openDefaultTideDb :: IO Bool
openDefaultTideDb = do
    filenames <- getDirectoryContents defaultTideDbPath
    let tcds = filter (".tcd" `isSuffixOf`) filenames
    if null tcds
        then return False
        else openTideDb $ defaultTideDbPath </> head tcds
