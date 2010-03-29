{-# LANGUAGE DeriveDataTypeable #-}
module Web.Routes.Quasi
    where

import Data.Object.String
import Data.Object.Yaml
import Data.Attempt
import Control.Monad
import qualified Safe.Failure as SF
import Data.Char
import Control.Arrow
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Data.ByteString.Char8 (pack)
import Data.Data

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

parseRoutes :: QuasiQuoter
parseRoutes = QuasiQuoter x y where
    x yaml = do
        resources <- qRunIO $ fa $ decode (pack yaml) >>= resourcesFromSO
        dataToExpQ (const Nothing) resources
    y yaml = do
        resources <- qRunIO $ fa $ decode (pack yaml) >>= resourcesFromSO
        dataToPatQ (const Nothing) resources
