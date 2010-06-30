{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Web.Routes.Quasi.Classes
    ( SinglePiece (..)
    , MultiPiece (..)
    , Strings
    ) where

import Data.Int (Int64)

class SinglePiece s where
    fromSinglePiece :: String -> Either String s
    toSinglePiece :: s -> String
instance SinglePiece String where
    fromSinglePiece = Right
    toSinglePiece = id
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
