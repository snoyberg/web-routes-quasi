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
import Web.Routes.Site

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

readMethod :: String -> Maybe Method
readMethod = SF.read

resourcesFromString :: String -> [Resource]
resourcesFromString = map go . filter (not . null) . lines where
    go s =
        case words s of
            (pattern:constr:rest) ->
                let pieces = piecesFromString $ drop1Slash pattern
                 in Resource constr pieces $ go' s constr rest
            _ -> error $ "Invalid resource line: " ++ s
    go' s constr rest =
        case mapM readMethod rest of
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
    go (Resource n pieces h) = NormalC (mkName n)
                             $ mapMaybe go' pieces
                            ++ go'' h
    go' (StringPiece _) = Just (NotStrict, ConT $ mkName "String")
    go' (IntPiece _) = Just (NotStrict, ConT $ mkName "Integer")
    go' (SlurpPiece _) = Just (NotStrict, AppT ListT $ ConT $ mkName "String")
    go' _ = Nothing
    go'' (SubSite t _) = [(NotStrict, ConT $ mkName t)]
    go'' _ = []
    claz = [mkName "Show", mkName "Read", mkName "Eq"]

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
    go (Resource n ps h) = do
        let ps' = zip [1..] ps
        let pat = mkPat ps' h
        bod <- foldM go' (ConE $ mkName n) ps'
        bod' <- case h of
                    SubSite _ f -> do
                        parse <- [|parsePathSegments|]
                        let parse' = parse `AppE` VarE (mkName f)
                        let rhs = parse' `AppE` VarE (mkName "var0")
                        fm <- [|fmapEither|]
                        return $ fm `AppE` bod `AppE` rhs
                    _ -> do
                        ri <- [|Right|]
                        return $ AppE ri bod
        return $ Clause [pat] (NormalB bod') []
    mkPat [] (SubSite _ _) = VarP $ mkName "var0"
    mkPat [] _ = ConP (mkName "[]") []
    mkPat ((_, StaticPiece t):rest) h =
        ConP (mkName ":") [ LitP (StringL t)
                          , mkPat rest h
                          ]
    mkPat ((i, SlurpPiece _):_) _ = VarP $ mkName $ "var" ++ show i
    mkPat ((i, _):rest) h = ConP (mkName ":")
        [ VarP $ mkName $ "var" ++ show (i :: Int)
        , mkPat rest h
        ]
    go' x (_, StaticPiece _) = return x
    go' x (i, SlurpPiece _) =
        return $ x `AppE` VarE (mkName $ "var" ++ show i)
    go' x (i, StringPiece _) =
        return $ x `AppE` VarE (mkName $ "var" ++ show i)
    go' x (i, IntPiece _) = do
        re <- [|read|] -- This is really bad...
        return $ x `AppE` (re `AppE` VarE (mkName $ "var" ++ show i))

fmapEither :: (a -> b) -> Either x a -> Either x b
fmapEither _ (Left x) = Left x
fmapEither f (Right a) = Right $ f a

renderDecType :: String -> Q Dec
renderDecType s =
    let str = ConT $ mkName "String"
        strl = ListT `AppT` str
        ret = ConT $ mkName s
        typ = ArrowT `AppT` ret `AppT` strl
     in return $ SigD (mkName $ "render" ++ s) typ

renderDec :: String -> [Resource] -> Q Dec
renderDec s res = FunD (mkName $ "render" ++ s) `fmap` mapM go res where
    go (Resource n ps h) = do
        let ps' = zip [1..] ps
        let pat = ConP (mkName n) $ mapMaybe go' ps' ++ lastPat h
        bod <- mkBod ps' h
        return $ Clause [pat] (NormalB bod) []
    lastPat (SubSite _ _) = [VarP $ mkName "var0"]
    lastPat _ = []
    go' (_, StaticPiece _) = Nothing
    go' (i, _) = Just $ VarP $ mkName $ "var" ++ show (i :: Int)
    mkBod [] (SubSite _ f) = do
        format <- [|formatPathSegments|]
        let format' = format `AppE` VarE (mkName f)
        return $ format' `AppE` VarE (mkName "var0")
    mkBod [] _ = lift ([] :: [String])
    mkBod ((_, StaticPiece x):xs) h = do
        x' <- lift x
        xs' <- mkBod xs h
        return $ ConE (mkName ":") `AppE` x' `AppE` xs'
    mkBod ((i, StringPiece _):xs) h = do
        let x' = VarE $ mkName $ "var" ++ show i
        xs' <- mkBod xs h
        return $ ConE (mkName ":") `AppE` x' `AppE` xs'
    mkBod ((i, IntPiece _):xs) h= do
        sh <- [|show|]
        let x' = AppE sh $ VarE $ mkName $ "var" ++ show i
        xs' <- mkBod xs h
        return $ ConE (mkName ":") `AppE` x' `AppE` xs'
    mkBod ((i, SlurpPiece _):_) _ = return $ VarE $ mkName $ "var" ++ show i

dispDecType :: String -> Name -> Name -> Q Dec
dispDecType s a p = do
    let m = ConT ''Method
        url = ConT $ mkName s
        str = ConT ''String
        rend = ArrowT `AppT` url `AppT` str
        ret1 = ArrowT `AppT` url `AppT` ConT a
        ret2 = ArrowT `AppT` rend `AppT` ret1
        ret3 = ArrowT `AppT` m `AppT` ret2
        ret4 = ArrowT `AppT` ConT p `AppT` ret3
        ret5 = ArrowT `AppT` ConT a `AppT` ret4
    return $ SigD (mkName $ "dispatch" ++ s) ret5

dispDec :: String -> [Resource] -> Q Dec
dispDec s r = do
    -- type: app -> param -> Method -> (url -> String) -> url -> app
    badMethod <- newName "badMethod"
    param <- newName "param"
    method <- newName "method"
    render <- newName "render"
    url <- newName "url"
    clauses <- mapM (go badMethod param method render url) r
    return $ FunD (mkName $ "dispatch" ++ s) $ clauses
  where
    go badMethod param method render url (Resource constr ps handler) = do
        conArgs <- go' ps handler
        let pat = [ VarP badMethod, VarP param, VarP method, VarP render,
                    ConP (mkName constr) $ map VarP conArgs]
        b <- case handler of
                Single s -> do
                    let base = VarE (mkName s) `AppE` VarE param
                                               `AppE` VarE render
                    foldM go'' base conArgs
                ByMethod methods -> do
                    matches <- forM methods $ \(m, f) -> do
                        let pat' = ConP (mkName $ show m) []
                        let base = VarE (mkName f) `AppE` VarE param
                                                   `AppE` VarE render
                        bod <- foldM go'' base conArgs
                        return $ Match pat' (NormalB bod) []
                    let final =
                            if length methods == 4
                                then []
                                else [Match WildP (NormalB $ VarE badMethod) []]
                    return $ CaseE (VarE method) $ matches ++ final
                _ -> return $ VarE badMethod -- FIXME
        return $ Clause pat (NormalB b) []
    go' [] (SubSite _ _) = do
        n <- newName "arg"
        return [n]
    go' [] _ = return []
    go' (StaticPiece _:rest) h = go' rest h
    go' (_:rest) h = do
        n <- newName "arg"
        ns <- go' rest h
        return $ n : ns
    go'' base arg = return $ base `AppE` VarE arg

createRoutes :: String -> Name -> Name -> [Resource] -> Q [Dec]
createRoutes name app param res = do
    dt <- dataTypeDec name res
    pat <- parseDecType name
    pa <- parseDec name res
    ret <- renderDecType name
    re <- renderDec name res
    dit <- dispDecType name app param
    di <- dispDec name res
    return [dt, pat, pa, ret, re, dit, di]
