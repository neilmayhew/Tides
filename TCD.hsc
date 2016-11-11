{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module TCD where

import Control.Applicative
import Data.Int
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

#include <tcd.h>

data DatabaseHeader = DatabaseHeader
    { hdrVersion          :: String
    , hdrMajorRev         :: Int
    , hdrMinorRev         :: Int
    , hdrLastModified     :: String
    , hdrNumberOfRecords  :: Int
    , hdrStartYear        :: Int
    , hdrNumberOfYears    :: Int
    , hdrConstituents     :: Int
    , hdrLevelUnitTypes   :: Int
    , hdrDirUnitTypes     :: Int
    , hdrRestrictionTypes :: Int
    , hdrDatumTypes       :: Int
    , hdrCountries        :: Int
    , hdrTzfiles          :: Int
    , hdrLegaleses        :: Int
    , hdrPedigreeTypes    :: Int
    } deriving (Eq, Show)

instance Storable DatabaseHeader where
    sizeOf _    = #{size DB_HEADER_PUBLIC}
    alignment _ = alignment (undefined :: Int32)
    peek hdr    = DatabaseHeader
        <$> (peekCString   $  (#{ptr  DB_HEADER_PUBLIC, version          } hdr             ))
        <*> (fromIntegral <$> (#{peek DB_HEADER_PUBLIC, major_rev        } hdr :: IO Word32))
        <*> (fromIntegral <$> (#{peek DB_HEADER_PUBLIC, minor_rev        } hdr :: IO Word32))
        <*> (peekCString   $  (#{ptr  DB_HEADER_PUBLIC, last_modified    } hdr             ))
        <*> (fromIntegral <$> (#{peek DB_HEADER_PUBLIC, number_of_records} hdr :: IO Word32))
        <*> (fromIntegral <$> (#{peek DB_HEADER_PUBLIC, start_year       } hdr :: IO Int32 ))
        <*> (fromIntegral <$> (#{peek DB_HEADER_PUBLIC, number_of_years  } hdr :: IO Word32))
        <*> (fromIntegral <$> (#{peek DB_HEADER_PUBLIC, constituents     } hdr :: IO Word32))
        <*> (fromIntegral <$> (#{peek DB_HEADER_PUBLIC, level_unit_types } hdr :: IO Word32))
        <*> (fromIntegral <$> (#{peek DB_HEADER_PUBLIC, dir_unit_types   } hdr :: IO Word32))
        <*> (fromIntegral <$> (#{peek DB_HEADER_PUBLIC, restriction_types} hdr :: IO Word32))
        <*> (fromIntegral <$> (#{peek DB_HEADER_PUBLIC, datum_types      } hdr :: IO Word32))
        <*> (fromIntegral <$> (#{peek DB_HEADER_PUBLIC, countries        } hdr :: IO Word32))
        <*> (fromIntegral <$> (#{peek DB_HEADER_PUBLIC, tzfiles          } hdr :: IO Word32))
        <*> (fromIntegral <$> (#{peek DB_HEADER_PUBLIC, legaleses        } hdr :: IO Word32))
        <*> (fromIntegral <$> (#{peek DB_HEADER_PUBLIC, pedigree_types   } hdr :: IO Word32))
    poke hdr _ = return ()

data TideRecordType = ReferenceStation | SubordinateStation | UndefinedStation
    deriving (Eq, Show, Enum)

instance Storable TideRecordType where
    sizeOf _    = sizeOf    (undefined :: CUChar)
    alignment _ = alignment (undefined :: CUChar)
    peek p      = do
        c <- peek (castPtr p :: Ptr CUChar)
        let t = case c of
                #{const REFERENCE_STATION  } -> ReferenceStation
                #{const SUBORDINATE_STATION} -> SubordinateStation
                _                            -> UndefinedStation
        return t
    poke p t    = do
        let c = case t of
                ReferenceStation   -> #{const REFERENCE_STATION  }
                SubordinateStation -> #{const SUBORDINATE_STATION}
        poke (castPtr p :: Ptr CUChar) c

data TideStationHeader = TideStationHeader
    { tshNumber           :: Int
    , tshSize             :: Int
    , tshType             :: TideRecordType
    , tshLatitude         :: Double
    , tshLongitude        :: Double
    , tshReferenceStation :: Int
    , tshTZFile           :: Int
    , tshName             :: String
    } deriving (Eq, Show)

instance Storable TideStationHeader where
    sizeOf _    = #{size TIDE_STATION_HEADER}
    alignment _ = alignment (undefined :: CDouble)
    peek hdr    = TideStationHeader
        <$> (fromIntegral <$> (#{peek TIDE_STATION_HEADER, record_number    } hdr :: IO Int32  ))
        <*> (fromIntegral <$> (#{peek TIDE_STATION_HEADER, record_size      } hdr :: IO Word32 ))
        <*> (                 (#{peek TIDE_STATION_HEADER, record_type      } hdr              ))
        <*> (realToFrac   <$> (#{peek TIDE_STATION_HEADER, latitude         } hdr :: IO CDouble))
        <*> (realToFrac   <$> (#{peek TIDE_STATION_HEADER, longitude        } hdr :: IO CDouble))
        <*> (fromIntegral <$> (#{peek TIDE_STATION_HEADER, reference_station} hdr :: IO Int32  ))
        <*> (fromIntegral <$> (#{peek TIDE_STATION_HEADER, tzfile           } hdr :: IO Int16  ))
        <*> (peekCString   $  (#{ptr  TIDE_STATION_HEADER, name             } hdr              ))
      where
        toRecordType :: CUChar -> TideRecordType
        toRecordType = toEnum . fromIntegral
    poke hdr _  = return ()

data TideRecord = TideRecord
    { trHeader            :: TideStationHeader
    , trCountry           :: Int
    , trSource            :: String
    , trRestriction       :: Int
    , trComments          :: String
    , trNotes             :: String
    , trLegalese          :: Int
    , trStationIdContext  :: String
    , trStationId         :: String
    , trDateImported      :: Int
    , trXfields           :: String
    , trDirectionUnits    :: Int
    , trMinDirection      :: Int
    , trMaxDirection      :: Int
    , trLevelUnits        :: Int
                          
    -- Type 1             
    , trDatumOffset       :: Double
    , trDatum             :: Int
    , trZoneOffset        :: Int
    , trExpirationDate    :: Int
    , trMonthsOnStation   :: Int
    , trLastDateOnStation :: Int
    , trConfidence        :: Int
    , trAmplitudes        :: [Double]
    , trEpochs            :: [Double]
                          
    -- Type 2             
    , trMinTimeAdd        :: Int
    , trMinLevelAdd       :: Double
    , trMinLevelMultiply  :: Double
    , trMaxTimeAdd        :: Int
    , trMaxLevelAdd       :: Double
    , trMaxLevelMultiply  :: Double
    , trFloodBegins       :: Int
    , trEbbBegins         :: Int
    } deriving (Eq, Show)

maxConstituents = #{const MAX_CONSTITUENTS}

instance Storable TideRecord where
    sizeOf _    = #{size TIDE_RECORD}
    alignment _ = alignment (undefined :: TideStationHeader)
    peek rec    = TideRecord
        <$> (                 (#{peek TIDE_RECORD, header              } rec :: IO TideStationHeader))
        <*> (fromIntegral <$> (#{peek TIDE_RECORD, country             } rec :: IO Int16            ))
        <*> (peekCString   $  (#{ptr  TIDE_RECORD, source              } rec                        ))
        <*> (fromIntegral <$> (#{peek TIDE_RECORD, restriction         } rec :: IO Word8            ))
        <*> (peekCString   $  (#{ptr  TIDE_RECORD, comments            } rec                        ))
        <*> (peekCString   $  (#{ptr  TIDE_RECORD, notes               } rec                        ))
        <*> (fromIntegral <$> (#{peek TIDE_RECORD, legalese            } rec :: IO Word8            ))
        <*> (peekCString   $  (#{ptr  TIDE_RECORD, station_id_context  } rec                        ))
        <*> (peekCString   $  (#{ptr  TIDE_RECORD, station_id          } rec                        ))
        <*> (fromIntegral <$> (#{peek TIDE_RECORD, date_imported       } rec :: IO Word32           ))
        <*> (peekCString   $  (#{ptr  TIDE_RECORD, xfields             } rec                        ))
        <*> (fromIntegral <$> (#{peek TIDE_RECORD, direction_units     } rec :: IO Word8            ))
        <*> (fromIntegral <$> (#{peek TIDE_RECORD, min_direction       } rec :: IO Int32            ))
        <*> (fromIntegral <$> (#{peek TIDE_RECORD, max_direction       } rec :: IO Int32            ))
        <*> (fromIntegral <$> (#{peek TIDE_RECORD, level_units         } rec :: IO Word8            ))
        <*> (realToFrac   <$> (#{peek TIDE_RECORD, datum_offset        } rec :: IO CFloat           ))
        <*> (fromIntegral <$> (#{peek TIDE_RECORD, datum               } rec :: IO Int16            ))
        <*> (fromIntegral <$> (#{peek TIDE_RECORD, zone_offset         } rec :: IO Int32            ))
        <*> (fromIntegral <$> (#{peek TIDE_RECORD, expiration_date     } rec :: IO Word32           ))
        <*> (fromIntegral <$> (#{peek TIDE_RECORD, months_on_station   } rec :: IO Word16           ))
        <*> (fromIntegral <$> (#{peek TIDE_RECORD, last_date_on_station} rec :: IO Word32           ))
        <*> (fromIntegral <$> (#{peek TIDE_RECORD, confidence          } rec :: IO Word8            ))
        <*> (peekConstituents (#{ptr  TIDE_RECORD, amplitude           } rec                        ))
        <*> (peekConstituents (#{ptr  TIDE_RECORD, epoch               } rec                        ))
        <*> (fromIntegral <$> (#{peek TIDE_RECORD, min_time_add        } rec :: IO Int32            ))
        <*> (realToFrac   <$> (#{peek TIDE_RECORD, min_level_add       } rec :: IO CFloat           ))
        <*> (realToFrac   <$> (#{peek TIDE_RECORD, min_level_multiply  } rec :: IO CFloat           ))
        <*> (fromIntegral <$> (#{peek TIDE_RECORD, max_time_add        } rec :: IO Int32            ))
        <*> (realToFrac   <$> (#{peek TIDE_RECORD, max_level_add       } rec :: IO CFloat           ))
        <*> (realToFrac   <$> (#{peek TIDE_RECORD, max_level_multiply  } rec :: IO CFloat           ))
        <*> (fromIntegral <$> (#{peek TIDE_RECORD, flood_begins        } rec :: IO Int32            ))
        <*> (fromIntegral <$> (#{peek TIDE_RECORD, ebb_begins          } rec :: IO Int32            ))
      where
        peekConstituents :: Ptr CFloat -> IO [Double]
        peekConstituents p = map realToFrac <$> peekArray maxConstituents p
    poke rec _  = return ()

{- DWF: This value signifies "null" or "omitted" slack offsets
   (flood_begins, ebb_begins).  Zero is *not* the same. -}
{- Time offsets are represented as hours * 100 plus minutes.
   0xA00 = 2560
   It turns out that offsets do exceed 24 hours (long story), but we
   should still be safe with the 60. -}

nullSlackOffset = #{const NULLSLACKOFFSET}

-- This is the level below which an amplitude rounds to zero.
-- It should be exactly (0.5 / DEFAULT_AMPLITUDE_SCALE).

amplitudeEpsilon = 0.00005 -- #{const AMPLITUDE_EPSILON}

openTideDb :: String -> IO Bool
openTideDb filepath = do
    withCString filepath $ \cstr ->
        toBool <$> c_open_tide_db cstr

foreign import ccall safe "tcd.h open_tide_db"
  c_open_tide_db :: Ptr CChar -> IO CUChar

closeTideDb :: IO ()
closeTideDb = c_close_tide_db

foreign import ccall safe "TCD.h close_tide_db"
  c_close_tide_db :: IO ()

getTideDbHeader :: IO DatabaseHeader
getTideDbHeader =
    alloca $ \prec -> do
        c_get_tide_db_header prec
        peek prec

foreign import ccall safe "TCDAux.h get_tide_db_header_"
  c_get_tide_db_header :: Ptr DatabaseHeader -> IO ()

{- Gets "header" portion of tide record for the station whose
   record_number is num [0,number_of_records-1] and writes it into
   rec.  Returns false if num is out of range. -}

getPartialTideRecord :: Int -> IO (Bool, TideStationHeader)
getPartialTideRecord num =
    alloca $ \prec -> do
        res <- (/= 0) <$> c_get_partial_tide_record (fromIntegral num) prec
        rec <- peek prec
        return (res, rec)

foreign import ccall safe "tcd.h get_partial_tide_record"
  c_get_partial_tide_record :: Int32 -> Ptr TideStationHeader -> IO Word8

readTideRecord :: Int -> IO (Int, TideRecord)
readTideRecord num =
    alloca $ \prec -> do
        res <- fromIntegral <$> c_read_tide_record (fromIntegral num) prec
        rec <- peek prec
        return (res, rec)

foreign import ccall safe "tcd.h read_tide_record"
  c_read_tide_record :: Int32 -> Ptr TideRecord -> IO Int32

{- Invokes get_partial_tide_record for a station that appears closest
   to the specified lat and lon in the Cylindrical Equidistant
   projection.  Returns the record number or -1 for failure. -}

getNearestPartialTideRecord :: Double -> Double -> IO (Int, TideStationHeader)
getNearestPartialTideRecord lat lon =
    alloca $ \prec -> do
        res <- fromIntegral <$> c_get_nearest_partial_tide_record
                    (realToFrac lat) (realToFrac lon) prec
        rec <- peek prec
        return (res, rec)

foreign import ccall safe "tcd.h get_nearest_partial_tide_record"
  c_get_nearest_partial_tide_record :: CDouble -> CDouble -> Ptr TideStationHeader -> IO Int32

dumpTideRecordNum :: Int -> IO ()
dumpTideRecordNum num =
    alloca $ \prec -> do
        c_read_tide_record (fromIntegral num) prec
        c_dump_tide_record prec

foreign import ccall safe "tcd.h dump_tide_record"
  c_dump_tide_record :: Ptr TideRecord -> IO ()

{- For fields in the tide record that are indices into tables of
   character string values, these functions are used to retrieve the
   character string value corresponding to a particular index.  The
   value "Unknown" is returned when no translation exists.  The return
   value is a pointer into static memory. -}

getCountry :: Int -> IO String
getCountry num = peekCString =<< c_get_country (fromIntegral num)

foreign import ccall safe "tcd.h get_country"
  c_get_country :: Int32 -> IO CString

getTZFile :: Int -> IO String
getTZFile num = peekCString =<< c_get_tzfile (fromIntegral num)

foreign import ccall safe "tcd.h get_tzfile"
  c_get_tzfile :: Int32 -> IO CString

getLevelUnits :: Int -> IO String
getLevelUnits num = peekCString =<< c_get_level_units (fromIntegral num)

foreign import ccall safe "tcd.h get_level_units"
  c_get_level_units :: Int32 -> IO CString

getDirUnits :: Int -> IO String
getDirUnits num = peekCString =<< c_get_dir_units (fromIntegral num)

foreign import ccall safe "tcd.h get_dir_units"
  c_get_dir_units :: Int32 -> IO CString

getRestriction :: Int -> IO String
getRestriction num = peekCString =<< c_get_restriction (fromIntegral num)

foreign import ccall safe "tcd.h get_restriction"
  c_get_restriction :: Int32 -> IO CString

getDatum :: Int -> IO String
getDatum num = peekCString =<< c_get_datum (fromIntegral num)

foreign import ccall safe "tcd.h get_datum"
  c_get_datum :: Int32 -> IO CString

getLegalese :: Int -> IO String
getLegalese num = peekCString =<< c_get_legalese (fromIntegral num)

foreign import ccall safe "tcd.h get_legalese"
  c_get_legalese :: Int32 -> IO CString

{- When invoked multiple times with the same string, returns record
   numbers of all stations that have that string anywhere in the
   station name.  This search is case insensitive.  When no more
   records are found it returns -1. -}

searchStation :: String -> IO Int
searchStation str = fromIntegral <$> withCString str c_search_station

foreign import ccall safe "tcd.h search_station"
  c_search_station :: CString -> IO Int32

{- Get the name of the constituent corresponding to index num
   [0,constituents-1].  The return value is a pointer into static
   memory. -}

getConstituent :: Int -> IO String
getConstituent num = peekCString =<< c_get_constituent (fromIntegral num)

foreign import ccall safe "tcd.h get_constituent"
  c_get_constituent :: Int32 -> IO CString

{- Get the name of the station whose record_number is num
   [0,number_of_records-1].  The return value is a pointer into static
   memory. -}

getStation :: Int -> IO String
getStation num = peekCString =<< c_get_station (fromIntegral num)

foreign import ccall safe "tcd.h get_station"
  c_get_station :: Int32 -> IO CString

{- Returns the speed of the constituent indicated by num
   [0,constituents-1]. -}

getSpeed :: Int -> IO Double
getSpeed num = realToFrac <$> c_get_speed (fromIntegral num)

foreign import ccall safe "tcd.h get_speed"
  c_get_speed :: Int32 -> IO CDouble

{- Get the equilibrium argument and node factor for the constituent
   indicated by num [0,constituents-1], for the year
   start_year+year. -}

getEquilibrium :: Int -> Int -> IO Double
getEquilibrium num year = realToFrac <$> c_get_equilibrium (fromIntegral num) (fromIntegral year)

foreign import ccall safe "tcd.h get_equilibrium"
  c_get_equilibrium :: Int32 -> Int32 -> IO CFloat

getNodeFactor :: Int -> Int -> IO Double
getNodeFactor num year = realToFrac <$> c_get_node_factor (fromIntegral num) (fromIntegral year)

foreign import ccall safe "tcd.h get_node_factor"
  c_get_node_factor :: Int32 -> Int32 -> IO CFloat

{- Get all available equilibrium arguments and node factors for the
   constituent indicated by num [0,constituents-1].  The return value
   is a pointer into static memory which is an array of
   number_of_years floats, corresponding to the years start_year
   through start_year+number_of_years-1. -}

getEquilibriums :: Int -> IO [Double]
getEquilibriums num = do
    n <- hdrNumberOfYears <$> getTideDbHeader
    a <- peekArray (fromIntegral n) =<< c_get_equilibriums (fromIntegral num)
    return $ map realToFrac a

foreign import ccall safe "tcd.h get_equilibriums"
  c_get_equilibriums :: Int32 -> IO (Ptr CFloat)

getNodeFactors  :: Int -> IO [Double]
getNodeFactors num = do
    n <- hdrNumberOfYears <$> getTideDbHeader
    a <- peekArray (fromIntegral n) =<< c_get_node_factors (fromIntegral num)
    return $ map realToFrac a

foreign import ccall safe "tcd.h get_node_factors"
  c_get_node_factors :: Int32 -> IO (Ptr CFloat)
