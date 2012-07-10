{-# LANGUAGE RecordWildCards #-}

module CV.Geo.Formats.GeoTiff where

import System.FilePath.Posix
import qualified CV.ImageMath as IM
import CV.Image
import CV.Geo.Types

-- | Loads Image GrayScale D32 from given filepath,
-- accompanied with .tfw world file.
loadGeoTiff :: FilePath -> IO GeoImage
loadGeoTiff fp = do
    x <- loadImage fp
    let worldFile = dropExtension fp ++ ".tfw"
    [a,b,c,d,e,f] <- readWorldFile worldFile
    return $ case x of
        Just im -> GeoImage{..}
        Nothing -> error ("File "++fp++" not found.")
  where
    readWorldFile :: FilePath -> IO [Double]
    readWorldFile = fmap (map read . lines . filter (/='\r')) . readFile