{-# LANGUAGE RecordWildCards #-}
module CV.Geo.Types where

import CV.Image
import Text.Printf

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
     , f :: Double
     }

instance Show GeoImage where
    show GeoImage{..} =
        let 
            dims  = getSize im
            fl :: Double -> String
            fl    = printf "%.3f"
        in concat
            ["Image size: "++show dims
            ,", reference pixel geo: "++show (fl e,fl f)
            ,", pixel geo sizes: "++show (fl a,fl d)
            ,", rotations: "++show (fl b,fl c)
            ]


both f (a,b) = (f a, f b)
