

module Main where

import qualified Data.ByteString.Char8 as B
import Data.Geolocation.GeoIP


main = simpleTest

simpleTest = do
  f <- B.lines `fmap` B.readFile "data/city_test.txt"
  let ips = map (head . B.split '#') f

  db <- openGeoDB memory_cache "/usr/local/share/GeoIP/GeoIPCity.dat"
  rs <- mapM (geoLocateByIPAddress db) ips
  putStrLn . show $ rs
  
