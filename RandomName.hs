{-# LANGUAGE OverloadedStrings #-}
module RandomName where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as C
import System.IO.MMap
import System.Random
import Control.Monad
import qualified Data.Text as T
import Data.Text.Encoding

worder :: IO (IO T.Text)
worder =  do
    bs <- mmapFileByteString "words.txt" Nothing
    let pick = do
            n <- randomRIO (0,B.length bs - 100) 
            let xs = filter (((&&) <$> (> 2) <*> (< 5)) . C.length) $ C.lines $  C.fromStrict $ B.drop n bs
            case xs of 
                (x:y:_) -> return y
                _ -> pick
    return $ decodeUtf8 <$> C.toStrict <$> C.intercalate "-" <$> replicateM 2 pick
