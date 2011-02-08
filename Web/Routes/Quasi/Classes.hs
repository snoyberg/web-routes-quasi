{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Web.Routes.Quasi.Classes
    ( SinglePiece (..)
    , MultiPiece (..)
    , Strings
    ) where

import Data.Int (Int64)
import qualified Data.Text as S
import qualified Data.Text.Lazy as L

class SinglePiece s where
    fromSinglePiece :: String -> Either String s
    toSinglePiece :: s -> String
instance SinglePiece String where
    fromSinglePiece s = if null s then Left "Empty string" else Right s
    toSinglePiece = id
instance SinglePiece S.Text where
    fromSinglePiece s = if null s then Left "Empty string" else Right (S.pack s)
    toSinglePiece = S.unpack
instance SinglePiece L.Text where
    fromSinglePiece s = if null s then Left "Empty string" else Right (L.pack s)
    toSinglePiece = L.unpack
instance SinglePiece Integer where
    fromSinglePiece s = case reads s of
                            (i, _):_ -> Right i
                            _ -> Left $ "Invalid integer: " ++ s
    toSinglePiece = show
instance SinglePiece Int where
    fromSinglePiece s = case reads s of
                            (i, _):_ -> Right i
                            _ -> Left $ "Invalid integer: " ++ s
    toSinglePiece = show
instance SinglePiece Int64 where
    fromSinglePiece s = case reads s of
                            (i, _):_ -> Right i
                            _ -> Left $ "Invalid integer: " ++ s
    toSinglePiece = show

class MultiPiece s where
    fromMultiPiece :: [String] -> Either String s
    toMultiPiece :: s -> [String]
instance MultiPiece [String] where
    fromMultiPiece = Right
    toMultiPiece = id
type Strings = [String]
instance MultiPiece [S.Text] where
    fromMultiPiece = Right . map S.pack
    toMultiPiece = map S.unpack
instance MultiPiece [L.Text] where
    fromMultiPiece = Right . map L.pack
    toMultiPiece = map L.unpack
