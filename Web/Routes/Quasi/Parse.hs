{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Web.Routes.Quasi.Parse
    ( -- * Quasi quoter
      parseRoutes
    , parseRoutesNoCheck
    , Resource (..)
    , Piece (..)
    ) where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Data.Data
import Data.Char
import Data.Maybe

-- | A quasi-quoter to parse a string into a list of 'Resource's. Checks for
-- overlapping routes, failing if present; use 'parseRoutesNoCheck' to skip the
-- checking. See documentation site for details on syntax.
parseRoutes :: QuasiQuoter
parseRoutes = QuasiQuoter x y where
    x s = do
        let res = resourcesFromString s
        case findOverlaps res of
            [] -> lift res
            _ -> error $ "Overlapping routes: " ++ unlines (map show res)
    y = dataToPatQ (const Nothing) . resourcesFromString

-- | Same as 'parseRoutes', but performs no overlap checking.
parseRoutesNoCheck :: QuasiQuoter
parseRoutesNoCheck = QuasiQuoter x y where
    x = lift . resourcesFromString
    y = dataToPatQ (const Nothing) . resourcesFromString

instance Lift Resource where
    lift (Resource s ps h) = do
        r <- [|Resource|]
        s' <- lift s
        ps' <- lift ps
        h' <- lift h
        return $ r `AppE` s' `AppE` ps' `AppE` h'

-- | A single resource pattern.
--
-- First argument is the name of the constructor, second is the URL pattern to
-- match, third is how to dispatch.
data Resource = Resource String [Piece] [String]
    deriving (Read, Show, Eq, Data, Typeable)

-- | A single piece of a URL, delimited by slashes.
--
-- In the case of StaticPiece, the argument is the value of the piece; for the
-- other constructors, it is the name of the parameter represented by this
-- piece. That value is not used here, but may be useful elsewhere.
data Piece = StaticPiece String
           | SinglePiece String
           | MultiPiece String
    deriving (Read, Show, Eq, Data, Typeable)

instance Lift Piece where
    lift (StaticPiece s) = do
        c <- [|StaticPiece|]
        s' <- lift s
        return $ c `AppE` s'
    lift (SinglePiece s) = do
        c <- [|SinglePiece|]
        s' <- lift s
        return $ c `AppE` s'
    lift (MultiPiece s) = do
        c <- [|MultiPiece|]
        s' <- lift s
        return $ c `AppE` s'

-- | Convert a multi-line string to a set of resources. See documentation for
-- the format of this string. This is a partial function which calls error on
-- invalid input.
resourcesFromString :: String -> [Resource]
resourcesFromString = map go . filter (not . null) . map trim . lines where
    go s =
        case words s of
            (pattern:constr:rest) ->
                let pieces = piecesFromString $ drop1Slash pattern
                 in Resource constr pieces rest
            _ -> error $ "Invalid resource line: " ++ s

-- | Drop leading whitespace.
trim :: String -> String
trim = dropWhile isSpace

drop1Slash :: String -> String
drop1Slash ('/':x) = x
drop1Slash x = x

piecesFromString :: String -> [Piece]
piecesFromString "" = []
piecesFromString x =
    let (y, z) = break (== '/') x
     in pieceFromString y : piecesFromString (drop1Slash z)

pieceFromString :: String -> Piece
pieceFromString ('#':x) = SinglePiece x
pieceFromString ('*':x) = MultiPiece x
pieceFromString x = StaticPiece x

findOverlaps :: [Resource] -> [(Resource, Resource)]
findOverlaps = gos . map justPieces
  where
    justPieces r@(Resource _ ps _) = (ps, r)
    gos [] = []
    gos (x:xs) = mapMaybe (go x) xs ++ gos xs
    go (StaticPiece x:xs, xr) (StaticPiece y:ys, yr)
        | x == y = go (xs, xr) (ys, yr)
        | otherwise = Nothing
    go (MultiPiece _:_, xr) (_, yr) = Just (xr, yr)
    go (_, xr) (MultiPiece _:_, yr) = Just (xr, yr)
    go ([], xr) ([], yr) = Just (xr, yr)
    go ([], _) (_, _) = Nothing
    go (_, _) ([], _) = Nothing
    go (_:xs, xr) (_:ys, yr) = go (xs, xr) (ys, yr)
