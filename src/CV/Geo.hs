{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module CV.Geo (
    GeoPoint
   ,Width
   ,GeoImage
   -- * Extractors
   ,chanValue
   ,getGeoRegion
   ,geo2Pixel
   ,pixel2Geo
   -- * File loading
   ,loadGeoTiff
--   ,loadENVI
    ) where

import System.FilePath.Posix
import CV.Geo.Projections
import qualified CV.ImageMath as IM
import CV.Geo.Formats.GeoTiff
import CV.Geo.Types
import CV.Image


chanValue :: GeoImage -> Width -> GeoPoint -> Double
chanValue i@GeoImage{..} width p =
    let region    = getGeoRegion i width p
    in realToFrac . IM.average $ region

-- | Returns sequare area with given width & height in meters
-- around given center
getGeoRegion :: GeoImage -> Width -> GeoPoint -> Image GrayScale D32
getGeoRegion i@GeoImage{..} width (c_x,c_y) =
    let inX = 0.0000036306 * 5 -- unit/m FIXME DIRTY HACK!
        inY = 0.00000179502 * 5 -- unit/m FIXME DIRTY HACK!
        w_2 = (inX * width) / 2
        h_2 = (inY * width) / 2
        (lu_x,lu_y) = geo2Pixel i (c_x-w_2, c_y-h_2)
        (rl_x,rl_y) = geo2Pixel i (c_x+w_2, c_y+h_2)
        w_p = rl_x - lu_x
        h_p = lu_y - rl_y
    in getRegion (lu_x,lu_y) (w_p,h_p) im

-- http://www.remotesensing.org/geotiff/faq.html#WorldFile1
geo2Pixel :: GeoImage -> GeoPoint -> (Int,Int)
geo2Pixel GeoImage{..} (x_geo,y_geo) =
    let pY :: Int
        pY  = ceiling $ (+0.5) $ (a*y_geo - a*f - b*x_geo + b*e) / (a*d - b*c)
        pX :: Int
        pX  = ceiling $ (+0.5) $ (x_geo - e - (fromIntegral pY)*c) / a
    in (pX,pY)

pixel2Geo :: GeoImage -> (Int,Int) -> GeoPoint
pixel2Geo GeoImage{..} (fromIntegral -> pX, fromIntegral -> pY) =
    let x_geo :: Double
        x_geo = e + a*pX + c*pY
        y_geo :: Double
        y_geo = f + d*pY + b*pX
    in (x_geo, y_geo)

