module Web.Routes.Quasi
    where

import Data.Object.String
import Data.Attempt
import Control.Monad
import qualified Safe.Failure as SF
import Data.Char
import Control.Arrow

-- | In theory could use the definition from WAI, but:
--
-- * that would introduce a dependency on WAI
--
-- * we want to constrain this to only certain methods
data Method = GET | PUT | POST | DELETE
    deriving (Read, Show, Eq)

data Resource = Resource String [Piece] Handler
    deriving (Read, Show, Eq)

data Handler = ByMethod [(Method, String)]
             | Single String
             | SubSite String String
    deriving (Read, Show, Eq)

data Piece = StaticPiece String
           | StringPiece String
           | IntPiece String
           | SlurpPiece String
    deriving (Read, Show, Eq)

resourcesFromSO :: StringObject -> Attempt [Resource]
resourcesFromSO = mapM go <=< fromMapping where
    go (pattern, body) = do
        let pieces = piecesFromString $ drop1Slash pattern
        m <- fromMapping body
        name <- lookupScalar "name" m
        h <- case lookup "methods" m of
          Just x -> do
            methods <- fromSequence x >>= mapM (SF.read <=< fromScalar)
            let funcs = map (id &&& (\y -> map toLower (show y) ++ name)) methods
            return $ ByMethod funcs
          Nothing -> case lookup "subsite" m of
            Just x -> do
              dt <- fromScalar x
              dis <- lookupScalar "dispatch" m
              return $ SubSite dt dis
            Nothing -> return $ Single $ "handler" ++ name
        return $ Resource name pieces h

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
