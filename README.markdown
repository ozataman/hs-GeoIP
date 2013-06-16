# hs-GeoIP: Geolocation in Haskell using MaxMind GeoIPCity 

This library provides fast, pure Haskell bindings to the MaxMind GeoIPCity
database and the C library.

The workflow mirrors the various MaxMind bindings to other languages with
a flavor of Haskell introduced for safe operation.

Here is an example session:

    geoLookup = do
      g <- openGeoDB  memory_cache "GeoLiteCity.dat"
      geoLocateByIPAddress g "5.5.5.5"


Please see haddocks for additional details.
