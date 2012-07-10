module CV.Geo.Types where

import CV.Image

-- | WGS84
type GeoPoint = (Double,Double)

-- | Meters
type Width = Double

-- | Basically tif and world file wrapper
-- http://www.remotesensing.org/geotiff/faq.html#WorldFile1
data GeoImage = GeoImage {
       im :: Image GrayScale D32
     , a :: Double
     , b :: Double
     , c :: Double
     , d :: Double
     , e :: Double
     , f :: Double }

