{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module CV.Geo.Formats.GeoTiff where

import System.FilePath.Posix
import qualified CV.ImageMath as IM
import CV.Image
import CV.Geo.Types

-- | Loads Image GrayScale D32 from given filepath,
-- accompanied with .tfw world file.
loadGeoTiff :: Loadable a => FilePath -> IO (GeoImage a)
loadGeoTiff (dropExtension->fp) = do
    im <- readFromFile (fp++".tif")
    let
        worldFile = dropExtension fp ++ ".tfw"
    [a,b,c,d,e,f] <- readWorldFile worldFile
    return GeoImage{..}
  where
    readWorldFile :: FilePath -> IO [Double]
    readWorldFile = fmap (map read . lines . filter (/='\r')) . readFile
