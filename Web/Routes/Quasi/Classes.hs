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
import qualified Data.Text.Read
import Web.PathPieces

type Strings = [String]
