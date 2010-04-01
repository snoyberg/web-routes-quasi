{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Web.Routes.Quasi
    where

import qualified Safe.Failure as SF
import Data.Char
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Data.Data
import Data.Maybe
import Control.Monad

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

parseDecType :: String -> Q Dec
parseDecType s =
    let str = ConT $ mkName "String"
        eit = ConT $ mkName "Either"
        ret = eit `AppT` str `AppT` (ConT $ mkName s)
        strl = ListT `AppT` str
        typ = ArrowT `AppT` strl `AppT` ret
     in return $ SigD (mkName $ "parse" ++ s) typ

parseDec :: String -> [Resource] -> Q Dec
parseDec s r = do
    final' <- final
    clauses <- mapM go r
    return $ FunD (mkName $ "parse" ++ s) $ clauses ++ [final']
  where
    msg = LitE $ StringL "Could not parse URL"
    final = do
        le <- [|Left|]
        return $ Clause [WildP] (NormalB $ AppE le msg) []
    go (Resource n ps _) = do
        let ps' = zip [1..] ps
        let pat = mkPat ps'
        bod <- foldM go' (ConE $ mkName n) ps'
        ri <- [|Right|]
        return $ Clause [pat] (NormalB $ AppE ri bod) []
    mkPat [] = ConP (mkName "[]") []
    mkPat ((_, StaticPiece t):rest) = ConP (mkName ":") [ LitP (StringL t)
                                                        , mkPat rest
                                                        ]
    mkPat ((i, SlurpPiece _):_) = VarP $ mkName $ "var" ++ show i
    mkPat ((i, _):rest) = ConP (mkName ":")
        [ VarP $ mkName $ "var" ++ show (i :: Int)
        , mkPat rest
        ]
    go' x (_, StaticPiece _) = return x
    go' x (i, SlurpPiece _) =
        return $ x `AppE` VarE (mkName $ "var" ++ show i)
    go' x (i, StringPiece _) =
        return $ x `AppE` VarE (mkName $ "var" ++ show i)
    go' x (i, IntPiece _) = do
        re <- [|read|] -- This is really bad...
        return $ x `AppE` (re `AppE` VarE (mkName $ "var" ++ show i))

renderDecType :: String -> Q Dec
renderDecType s =
    let str = ConT $ mkName "String"
        strl = ListT `AppT` str
        ret = ConT $ mkName s
        typ = ArrowT `AppT` ret `AppT` strl
     in return $ SigD (mkName $ "render" ++ s) typ

renderDec :: String -> [Resource] -> Q Dec
renderDec s res = FunD (mkName $ "render" ++ s) `fmap` mapM go res where
    go (Resource n ps _) = do
        let ps' = zip [1..] ps
        let pat = ConP (mkName n) $ mapMaybe go' ps'
        bod <- mkBod ps'
        return $ Clause [pat] (NormalB bod) []
    go' (_, StaticPiece _) = Nothing
    go' (i, _) = Just $ VarP $ mkName $ "var" ++ show (i :: Int)
    mkBod [] = lift ([] :: [String])
    mkBod ((_, StaticPiece x):xs) = do
        x' <- lift x
        xs' <- mkBod xs
        return $ ConE (mkName ":") `AppE` x' `AppE` xs'
    mkBod ((i, StringPiece _):xs) = do
        let x' = VarE $ mkName $ "var" ++ show i
        xs' <- mkBod xs
        return $ ConE (mkName ":") `AppE` x' `AppE` xs'
    mkBod ((i, IntPiece _):xs) = do
        sh <- [|show|]
        let x' = AppE sh $ VarE $ mkName $ "var" ++ show i
        xs' <- mkBod xs
        return $ ConE (mkName ":") `AppE` x' `AppE` xs'
    mkBod ((i, SlurpPiece _):_) = return $ VarE $ mkName $ "var" ++ show i

createRoutes :: String -> [Resource] -> Q [Dec]
createRoutes name res = do
    dt <- dataTypeDec name res
    pat <- parseDecType name
    pa <- parseDec name res
    ret <- renderDecType name
    re <- renderDec name res
    return [dt, pat, pa, ret, re]
