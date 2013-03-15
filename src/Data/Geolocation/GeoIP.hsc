{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Data.Geolocation.GeoIP 

    (
    -- * Types
      GeoDB
    , GeoIPOption
    , combineOptions
    , standard
    , memory_cache
    , check_cache
    , index_cache
    , mmap_cache
    
    -- * Geolocation Result Type
    , GeoIPRecord(..)
                 
    -- * Data Operations
    , openGeoDB
    , geoLocateByIPAddress
    , geoLocateByIPNum
    , geoStringByIPAddress
    , geoStringByIPNum
    , mkIpNum
    ) where

-------------------------------------------------------------------------------
import Control.Applicative
import Control.DeepSeq
import Data.ByteString.Char8 (ByteString, packCString, split, unpack)
import Foreign
import Foreign.C.String 
import Foreign.C.Types 
import Foreign.Ptr 
-------------------------------------------------------------------------------


#include "GeoIP.h"
#include "GeoIPCity.h"


data GeoIP

------------------------------------------------------------------------------
-- | Type representing an established connection to a GeoIPCity database
newtype GeoDB = GeoDB { unGeoDB :: ForeignPtr GeoIP }


------------------------------------------------------------------------------
-- | Return data for a geolocation lookup 
data GeoIPRecord = GeoIPRecord
  { geoCountryCode :: !ByteString
  , geoCountryCode3 :: !ByteString
  , geoCountryName :: !ByteString
  , geoRegion :: !ByteString
  , geoCity :: !ByteString
  , geoPostalCode :: !ByteString
  , geoLatitude :: !Double
  , geoLongitude :: !Double
  , geoAreaCode :: !Int
  , geoContinentCode :: !ByteString
  , geoAccuracyRadius :: !Int
  } deriving (Eq, Show)


instance NFData GeoIPRecord where
  rnf a = a `seq` ()


peekGeoIPRecord :: Ptr GeoIPRecord -> IO (Maybe GeoIPRecord)
peekGeoIPRecord p = 
  case nullPtr == p of
    True -> return Nothing
    False -> fmap Just r 
  where
    !r = GeoIPRecord
          <$> peekBS (#{peek GeoIPRecord, country_code})
          <*> peekBS (#{peek GeoIPRecord, country_code3})
          <*> peekBS (#{peek GeoIPRecord, country_name})
          <*> peekBS (#{peek GeoIPRecord, region})
          <*> peekBS (#{peek GeoIPRecord, city})
          <*> peekBS (#{peek GeoIPRecord, postal_code})
          <*> fmap tofloat (#{peek GeoIPRecord, latitude} p)
          <*> fmap tofloat (#{peek GeoIPRecord, longitude} p)
          <*> fmap toInt (#{peek GeoIPRecord, area_code} p)
          <*> peekBS (#{peek GeoIPRecord, continent_code})
          <*> pure 0
    peekBS f = do
      !sptr <- f p
      case nullPtr == sptr of
        True -> return ""
        False -> let x = packCString sptr in x `seq` x
    tofloat :: CFloat -> Double
    tofloat = realToFrac
    toInt :: CInt -> Int
    toInt = fromIntegral


newtype GeoIPOption = GeoIPOption { unGeoIPOpt :: CInt }


#{enum GeoIPOption, GeoIPOption
 , standard       = GEOIP_STANDARD
 , memory_cache   = GEOIP_MEMORY_CACHE
 , check_cache    = GEOIP_CHECK_CACHE
 , index_cache    = GEOIP_INDEX_CACHE
 , mmap_cache     = GEOIP_MMAP_CACHE 
 }


------------------------------------------------------------------------------
-- | Collapse & combine multiple 'GeoIPOption's into one
combineOptions :: [GeoIPOption] -> GeoIPOption 
combineOptions = GeoIPOption . foldr ((.|.) . unGeoIPOpt) 0


------------------------------------------------------------------------------
-- Utils


------------------------------------------------------------------------------
-- | Convert a string IP adress to IPNum
mkIpNum :: ByteString -> Maybe Integer
mkIpNum x = case valid of
  False -> Nothing
  True -> Just $ a * 16777216 + b * 65536 + 256 * c + d
  where
    valid = length parts == 4 && foldr (\x acc -> acc && x <= 255) True [a,b,c,d]
    a : b : c : d : _ = map (read . unpack) parts
    parts = split '.' x


------------------------------------------------------------------------------
-- Higher level GeoIP ops
--


------------------------------------------------------------------------------
-- | Open the binary GeoIP data file with the given options.
--
-- This would open a file and cache in memory:
--
-- > openGeoDB "GeoCity.dat" memory_cache
-- 
-- The memory on the C side is automatically freed by the Haskell GC when
-- appropriate.
openGeoDB :: GeoIPOption -> String -> IO GeoDB
openGeoDB ops dbname = withCString dbname $ \dbname' -> do
  ptr <- c_GeoIP_open dbname' ops
  GeoDB <$> newForeignPtr c_GeoIP_delete ptr


------------------------------------------------------------------------------
-- | Geo-locate by given IP Adress
--
-- > geoLocateByIPAddress db "123.123.123.123"
geoLocateByIPAddress :: GeoDB -> ByteString -> IO (Maybe GeoIPRecord)
geoLocateByIPAddress db ip =
  case mkIpNum ip of
    Nothing -> return Nothing
    Just inum -> geoLocateByIPNum db inum

geoStringByIPAddress :: GeoDB -> ByteString -> IO (Maybe ByteString)
geoStringByIPAddress db ip = 
  case mkIpNum ip of
    Nothing -> return Nothing
    Just inum -> geoStringByIPNum db inum

geoStringByIPNum :: GeoDB -> Integer -> IO (Maybe ByteString)
geoStringByIPNum (GeoDB db) ip =
  withForeignPtr db $ \db' -> do
    ptr <- c_GeoIP_name_by_ipnum db' (fromIntegral ip)
    str <- if nullPtr == ptr
           then return Nothing
           else let x = packCString ptr in x `seq` fmap Just x
    free ptr
    return str

------------------------------------------------------------------------------
-- | Geo-locate by given IP number. Call 'mkIpNum' on a 'String' ip address to
-- convert to IP number.
--
-- > geoLocateByIPNum db 12336939327338
geoLocateByIPNum :: GeoDB -> Integer -> IO (Maybe GeoIPRecord)
geoLocateByIPNum (GeoDB db) ip =
  withForeignPtr db $ \db' -> do
    ptr <- c_GeoIP_record_by_ipnum db' (fromIntegral ip)
    rec <- peekGeoIPRecord ptr
    return $ rec `deepseq` ()
    case ptr == nullPtr of
      True -> return ()
      False -> c_GeoIPRecord_delete ptr 
    return rec


------------------------------------------------------------------------------
-- Low level calls into the C library

foreign import ccall safe "GeoIP.h GeoIP_new"
  c_GeoIP_new 
    :: GeoIPOption 
    -> IO (Ptr GeoIP)


foreign import ccall safe "GeoIP.h &GeoIP_delete"
  c_GeoIP_delete 
    :: FunPtr (Ptr GeoIP -> IO ())


foreign import ccall safe "GeoIP.h GeoIP_open"
  c_GeoIP_open 
    :: CString
    -> GeoIPOption 
    -> IO (Ptr GeoIP)


foreign import ccall safe "GeoIPCity.h GeoIP_record_by_ipnum"
  c_GeoIP_record_by_ipnum 
    :: Ptr GeoIP
    -> CULong
    -> IO (Ptr GeoIPRecord)

foreign import ccall safe "GeoIP.h GeoIP_name_by_ipnum"
  c_GeoIP_name_by_ipnum
    :: Ptr GeoIP
    -> CULong
    -> IO CString

foreign import ccall safe "GeoIPCity.h &GeoIPRecord_delete"
  c_GeoIPRecord_delete_funPtr
    :: FunPtr (Ptr GeoIPRecord -> IO ())


foreign import ccall safe "GeoIPCity.h GeoIPRecord_delete"
  c_GeoIPRecord_delete 
    :: Ptr GeoIPRecord -> IO ()
