{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Web.Routes.Quasi.Parse
    ( -- * Quasi quoter
      parseRoutes
    , parseRoutesNoCheck
    , Resource (..)
    , Handler (..)
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
data Resource = Resource String [Piece] Handler
    deriving (Read, Show, Eq, Data, Typeable)

-- | Defines how to dispatch a request for a specific resource.
--
-- ByMethod allows a different function to be called for each request method.
-- The first value in each pair is the method, the second is the name of the
-- handler.
--
-- Single dispatches to a single function for all methods.
--
-- SubSite passes dispatch to a different site. The first argument is the name
-- of the datatype for the routes. The second is a function returning a
-- 'QuasiSite' for that type of routes. The third is a function converting the
-- master argument to the subsite argument.
data Handler = ByMethod [(String, String)] -- ^ (method, handler)
             | Single String
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

instance Lift Handler where
    lift (ByMethod s) = do
        c <- [|ByMethod|]
        s' <- lift s
        return $ c `AppE` s'
    lift (Single s) = do
        c <- [|Single|]
        s' <- lift s
        return $ c `AppE` s'

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
                    handler = go' constr rest
                 in if all isStatic pieces || True -- not (isSubSite handler)
                        then Resource constr pieces handler
                        else error "Subsites must have static pieces"
            _ -> error $ "Invalid resource line: " ++ s
    go' constr [] = Single $ "handle" ++ constr
    go' constr rest = ByMethod $ map helper rest
      where
        helper x =
            case break (== ':') x of
                (method, ':' : func) -> (method, func)
                _ -> (x, map toLower x ++ constr)

isStatic :: Piece -> Bool
isStatic (StaticPiece _) = True
isStatic _ = False

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
