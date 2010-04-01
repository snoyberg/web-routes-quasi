{-# LANGUAGE DeriveDataTypeable #-}
module Web.Routes.Quasi
    where

import qualified Safe.Failure as SF
import Data.Char
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Data.Data
import Data.Maybe

-- | In theory could use the definition from WAI, but:
--
-- * that would introduce a dependency on WAI
--
-- * we want to constrain this to only certain methods
data Method = GET | PUT | POST | DELETE
    deriving (Read, Show, Eq, Data, Typeable)

data Resource = Resource String [Piece] Handler
    deriving (Read, Show, Eq, Data, Typeable)

data Handler = ByMethod [(Method, String)]
             | Single String
             | SubSite String String
    deriving (Read, Show, Eq, Data, Typeable)

data Piece = StaticPiece String
           | StringPiece String
           | IntPiece String
           | SlurpPiece String
    deriving (Read, Show, Eq, Data, Typeable)

resourcesFromString :: String -> [Resource]
resourcesFromString = map go . filter (not . null) . lines where
    go s =
        case words s of
            (pattern:constr:rest) ->
                let pieces = piecesFromString $ drop1Slash pattern
                 in Resource constr pieces $ go' s constr rest
            _ -> error $ "Invalid resource line: " ++ s
    go' s constr rest =
        case mapM SF.read rest of
            Just [] -> Single $ "handle" ++ constr
            Just x -> ByMethod
                    $ map (\y -> (y, (map toLower $ show y) ++ constr)) x
            Nothing ->
                case rest of
                    [routes, getSite] -> SubSite routes getSite
                    _ -> error $ "Invalid resource line: " ++ s

drop1Slash :: String -> String
drop1Slash ('/':x) = x
drop1Slash x = x

piecesFromString :: String -> [Piece]
piecesFromString "" = []
piecesFromString x =
    let (y, z) = break (== '/') x
     in pieceFromString y : piecesFromString (drop1Slash z)

pieceFromString :: String -> Piece
pieceFromString ('$':x) = StringPiece x
pieceFromString ('#':x) = IntPiece x
pieceFromString ('*':x) = SlurpPiece x
pieceFromString x = StaticPiece x

parseRoutes :: QuasiQuoter
parseRoutes = QuasiQuoter x y where
    x = dataToExpQ (const Nothing) . resourcesFromString
    y = dataToPatQ (const Nothing) . resourcesFromString

dataTypeDec :: String -> [Resource] -> Q Dec
dataTypeDec name res = return $ DataD [] (mkName name) [] (map go res) claz
  where
    go (Resource n pieces _) = NormalC (mkName n) $ mapMaybe go' pieces
    go' (StringPiece _) = Just (NotStrict, ConT $ mkName "String")
    go' (IntPiece _) = Just (NotStrict, ConT $ mkName "Integer")
    go' (SlurpPiece _) = Just (NotStrict, AppT ListT $ ConT $ mkName "String")
    go' _ = Nothing
    claz = [mkName "Show", mkName "Read"]

createRoutes :: String -> [Resource] -> Q [Dec]
createRoutes name res = do
    dt <- dataTypeDec name res
    return [dt]
