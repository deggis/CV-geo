{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module CV.Geo.Formats.ENVI where

import System.FilePath.Posix
import qualified System.Posix.Files as F
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
                         , fileType :: Int
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
    hdrLines <- T.lines <$> T.readFile (path++".hdr")
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
    fileSize <- (fromIntegral . F.fileSize) <$> F.getFileStatus fp
    if fileSize `rem` (w*h) /= 0
        then error "ENVI file has incorrect filesize; skipping headers not supported!"
        else do
            let
                bytes = fileSize `div` (w*h)
                im = case bytes of
                    4 -> copyFCArrayToImage
                         . fromJust
                         . unsafeByteStringToCArray ((0,0),(w-1,h-1))
                         $ bs
                    5 -> copyCArrayToImage
                         . fromJust
                         . unsafeByteStringToCArray ((0,0),(w-1,h-1))
                         $ bs
                    _ -> error "Unsupported ENVI file type."
                -- If yPixel is positive, image is "upside down" i.e. the
                -- reference pixel is bottom left.
                fix = if yPixelSize > 0 then id else id
            return . fix $ im

-- Parses some header info from ENVI header file
-- http://geol.hu/data/online_help/ENVI_Header_Format.html
parseHeader :: [T.Text] -> Either String ENVIInfo
parseHeader rows = do
    h <- getRow "lines" >>= int'
    w <- getRow "samples" >>= int'
    fileType <- getRow "file type" >>= int'
    [_,refXLoc,refYLoc,east,north,xSize,ySize,_] <- getRow "map info" >>= dropBrackets >>= splitCommas
    fuu <- getRow "map info" >>= dropBrackets >>= splitCommas
    refPixelX <- int' refXLoc
    refPixelY <- int' refYLoc
    refPixelEasting <- double' east
    refPixelNorthing <- double' north
    xPixelSize <- double' xSize
    yPixelSize <- double' ySize
    return ENVIInfo{..}
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
