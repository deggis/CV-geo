{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module CV.Geo.Formats.ENVI where

import System.FilePath.Posix
import qualified CV.ImageMath as IM
import qualified Data.Attoparsec.Text as P
import qualified Data.Attoparsec.Combinator as P
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString as B
import Data.Array.CArray

import CV.Transforms
import CV.Conversions
import CV.Image
import CV.Geo.Types
import Data.Maybe
import Debug.Trace

data ENVIInfo = ENVIInfo { h :: Int
                         , w :: Int
                         , refPixelX :: Int
                         , refPixelY :: Int
                         , refPixelEasting :: Double
                         , refPixelNorthing :: Double
                         , xPixelSize :: Double
                         , yPixelSize :: Double
                         }
    deriving (Show)

loadENVI :: FilePath -> IO GeoImage
loadENVI (dropExtension->path) = do
    hdrLines <- fmap (T.lines) $ T.readFile (path++".hdr")
    case parseHeader hdrLines of
        Left s -> error s
        Right e@ENVIInfo{..} -> do
            im <- loadENVIImage e (path++".envi")
            let a = xPixelSize
                b = 0 -- ENVI rotations not supported
                c = 0
                d = yPixelSize
                e = refPixelEasting
                f = refPixelNorthing
            return GeoImage{..}

loadENVIImage :: ENVIInfo -> FilePath -> IO (Image GrayScale D32)
loadENVIImage ENVIInfo{..} fp = do
    bs <- B.readFile fp
    let ar = fromJust $ unsafeByteStringToCArray ((0,0),(w-1,h-1)) bs
        im = copyFCArrayToImage $ ar
        -- it seems image is read in wrong order and need to be rotated!
        -- FIXME: therefore reading non-square ENVI is likely to break
        -- Cheap, cheap fix.
        fix = CV.Transforms.flip Vertical . rotate (pi/2)
    return . fix $ im

-- Parses some header info from ENVI header file
-- http://geol.hu/data/online_help/ENVI_Header_Format.html
parseHeader :: [T.Text] -> Either String ENVIInfo
parseHeader rows = do
    h <- getRow "lines" >>= int'
    w <- getRow "samples" >>= int'
    [_,refXLoc,refYLoc,east,north,xSize,ySize,_] <- getRow "map info" >>= dropBrackets >>= splitCommas
    fuu <- getRow "map info" >>= dropBrackets >>= splitCommas
    refPixelX <- int' refXLoc
    refPixelY <- int' refYLoc
    refPixelEasting <- double' east
    refPixelNorthing <- double' north
    xPixelSize <- double' xSize
    yPixelSize <- double' ySize
    return . traceShow fuu $ ENVIInfo{..}
  where
    getRow name = P.parseOnly getValue
                . head
                . filter (T.isInfixOf $ T.pack name)
                $ rows
    getValue = (P.manyTill P.anyChar (P.char '=')) *> P.skipSpace *> P.takeText
    dropBrackets = P.parseOnly (P.char '{' *> P.takeWhile (/='}') <* P.char '}')
    splitCommas = P.parseOnly (P.many1 (P.takeWhile (/=',') <* P.char ',' <* P.skipSpace))

int' = P.parseOnly P.decimal
double' = P.parseOnly P.double

pS = P.string . T.pack

