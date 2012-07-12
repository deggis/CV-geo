{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module CV.Geo (
    GeoPoint
   ,Width
   ,GeoImage(..)
   -- * Extractors
   ,chanValue
   ,getGeoRegion
   ,getBoundingGeoBox
   ,getWGS84MetricBox
   ,geo2Pixel
   ,pixel2Geo
   -- * File loading
   ,loadGeoTiff
   ,loadENVI
    ) where

import System.FilePath.Posix
import CV.Geo.Projections
import qualified CV.ImageMath as IM
import CV.Geo.Formats.GeoTiff
import CV.Geo.Formats.ENVI
import CV.Geo.Types
import CV.Image


chanValue :: GeoImage -> Width -> GeoPoint -> Double
chanValue i@GeoImage{..} width p =
    let region    = getWGS84MetricBox i width p
    in realToFrac . IM.average $ region

-- http://www.remotesensing.org/geotiff/faq.html#WorldFile1
geo2Pixel :: GeoImage -> GeoPoint -> (Int,Int)
geo2Pixel GeoImage{..} (x_geo,y_geo) =
    let pY :: Int
        pY  = floor $ (+0.5) $ (a*y_geo - a*f - b*x_geo + b*e) / (a*d - b*c)
        pX :: Int
        pX  = floor $ (+0.5) $ (x_geo - e - (fromIntegral pY)*c) / a
    in (pX,pY)

pixel2Geo :: GeoImage -> (Int,Int) -> GeoPoint
pixel2Geo GeoImage{..} (fromIntegral -> pX, fromIntegral -> pY) =
    let x_geo :: Double
        x_geo = e + a*pX + c*pY
        y_geo :: Double
        y_geo = f + d*pY + b*pX
    in (x_geo, y_geo)

getBoundingGeoBox :: GeoImage -> (GeoPoint,GeoPoint)
getBoundingGeoBox GeoImage{..} =
    let
        (w,h) = both fromIntegral . getSize $ im
        rlx   = e + (w*a)
        rly   = f + (h*d)
    in ((e,f),(rlx,rly))

getGeoRegion :: (GeoPoint,GeoPoint) -> GeoImage -> GeoImage
getGeoRegion (lu@(lux,luy),rl@(rlx,rly)) g@GeoImage{..} =
    let
        plu@(plux,pluy) = geo2Pixel g lu
        prl   = geo2Pixel g rl
        (h,w) = size' plu prl
        reg   = getRegion (plux,pluy) (h,w) im
        dx    = a
        rot1  = b
        rot2  = c
        dy    = d
        refX  = e + (fromIntegral plux * a)
        refY  = f - (fromIntegral pluy * d)
    in GeoImage reg dx rot1 rot2 dy refX refY

size' :: Num a => (a,a) -> (a,a) -> (a,a)
size' lu@(a,b) rl@(c,d) = (abs (c-a),abs (b-d))

-- | Returns sequare area with given width & height in meters
-- around given center
getWGS84MetricBox :: GeoImage -> Width -> GeoPoint -> Image GrayScale D32
getWGS84MetricBox i@GeoImage{..} width (c_x,c_y) =
    let inX = 0.0000036306 * 5 -- unit/m FIXME DIRTY HACK!
        inY = 0.00000179502 * 5 -- unit/m FIXME DIRTY HACK!
        w_2 = (inX * width) / 2
        h_2 = (inY * width) / 2
        (lu_x,lu_y) = geo2Pixel i (c_x-w_2, c_y-h_2)
        (rl_x,rl_y) = geo2Pixel i (c_x+w_2, c_y+h_2)
        w_p = rl_x - lu_x
        h_p = lu_y - rl_y
    in getRegion (lu_x,lu_y) (w_p,h_p) im

