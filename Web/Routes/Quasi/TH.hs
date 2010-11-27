{-# LANGUAGE TemplateHaskell #-}
module Web.Routes.Quasi.TH
    ( createRoutes
    , createRender
    , createParse
    , createDispatch
    , Pieces (..)
    , THResource
    ) where

import Web.Routes.Quasi.Parse
import Web.Routes.Quasi.Classes
import Language.Haskell.TH.Syntax
import Data.Maybe
import Data.Either
import Data.List
import Data.Char (toLower)

data Pieces =
    SubSite
        { ssType :: Type
        , ssParse :: Exp
        , ssRender :: Exp
        , ssDispatch :: Exp
        , ssToMasterArg :: Exp
        , ssPieces :: [Piece]
        }
  | Simple [Piece] [String] -- ^ methods
type THResource = (String, Pieces)

createRoutes :: [THResource] -> Q [Con]
createRoutes res =
    return $ map go res
  where
    go (n, SubSite{ssType = s, ssPieces = pieces}) =
        NormalC (mkName n) $ mapMaybe go' pieces ++ [(NotStrict, s)]
    go (n, Simple pieces _) = NormalC (mkName n) $ mapMaybe go' pieces
    go' (SinglePiece x) = Just (NotStrict, ConT $ mkName x)
    go' (MultiPiece x) = Just (NotStrict, ConT $ mkName x)
    go' (StaticPiece _) = Nothing

-- | Generates the set of clauses necesary to parse the given 'Resource's. See 'quasiParse'.
createParse :: [THResource] -> Q [Clause]
createParse res = do
    final' <- final
    clauses <- mapM go res
    return $ if areResourcesComplete res
                then clauses
                else clauses ++ [final']
  where
    cons x y = ConP (mkName ":") [x, y]
    go (constr, SubSite{ssParse = p, ssPieces = ps}) = do
        ri <- [|Right|]
        be <- [|ape|]
        (pat', parse) <- mkPat' be ps $ ri `AppE` ConE (mkName constr)
        
        x <- newName "x"
        let pat = init pat' ++ [VarP x]

        --let pat = foldr (\a b -> cons [LitP (StringL a), b]) (VarP x) pieces
        let eitherSub = p `AppE` VarE x
        let bod = be `AppE` parse `AppE` eitherSub
        --let bod = fmape' `AppE` ConE (mkName constr) `AppE` eitherSub
        return $ Clause [foldr1 cons pat] (NormalB bod) []
    go (n, Simple ps _) = do
        ri <- [|Right|]
        be <- [|ape|]
        (pat, parse) <- mkPat' be ps $ ri `AppE` ConE (mkName n)
        return $ Clause [foldr1 cons pat] (NormalB parse) []
    final = do
        no <- [|Left "Invalid URL"|]
        return $ Clause [WildP] (NormalB no) []
    mkPat' :: Exp -> [Piece] -> Exp -> Q ([Pat], Exp)
    mkPat' be [MultiPiece s] parse = do
        v <- newName $ "var" ++ s
        fmp <- [|fromMultiPiece|]
        let parse' = InfixE (Just parse) be $ Just $ fmp `AppE` VarE v
        return ([VarP v], parse')
    mkPat' _ (MultiPiece _:_) _parse = error "MultiPiece must be last"
    mkPat' be (StaticPiece s:rest) parse = do
        (x, parse') <- mkPat' be rest parse
        let sp = LitP $ StringL s
        return (sp : x, parse')
    mkPat' be (SinglePiece s:rest) parse = do
        fsp <- [|fromSinglePiece|]
        v <- newName $ "var" ++ s
        let parse' = InfixE (Just parse) be $ Just $ fsp `AppE` VarE v
        (x, parse'') <- mkPat' be rest parse'
        return (VarP v : x, parse'')
    mkPat' _ [] parse = return ([ListP []], parse)

fmape :: (a -> b) -> Either String a -> Either String b
fmape _ (Left s) = Left s
fmape f (Right a) = Right $ f a

-- | 'ap' for 'Either'
ape :: Either String (a -> b) -> Either String a -> Either String b
ape (Left e) _ = Left e
ape (Right _) (Left e) = Left e
ape (Right f) (Right a) = Right $ f a

-- | Generates the set of clauses necesary to render the given 'Resource's. See
-- 'quasiRender'.
createRender :: [THResource] -> Q [Clause]
createRender = mapM go
  where
    go (n, Simple ps _) = do
        let ps' = zip [1..] ps
        let pat = ConP (mkName n) $ mapMaybe go' ps'
        bod <- mkBod ps'
        return $ Clause [pat] (NormalB $ TupE [bod, ListE []]) []
    go (n, SubSite{ssRender = r, ssPieces = pieces}) = do
        cons' <- [|\a (b, c) -> (a ++ b, c)|]
        let cons a b = cons' `AppE` a `AppE` b
        x <- newName "x"
        let r' = r `AppE` VarE x
        let pieces' = zip [1..] pieces
        let pat = ConP (mkName n) $ mapMaybe go' pieces' ++ [VarP x]
        bod <- mkBod pieces'
        return $ Clause [pat] (NormalB $ cons bod r') []
    go' (_, StaticPiece _) = Nothing
    go' (i, _) = Just $ VarP $ mkName $ "var" ++ show (i :: Int)
    mkBod [] = lift ([] :: [String])
    mkBod ((_, StaticPiece x):xs) = do
        x' <- lift x
        xs' <- mkBod xs
        return $ ConE (mkName ":") `AppE` x' `AppE` xs'
    mkBod ((i, SinglePiece _):xs) = do
        let x' = VarE $ mkName $ "var" ++ show i
        tsp <- [|toSinglePiece|]
        let x'' = tsp `AppE` x'
        xs' <- mkBod xs
        return $ ConE (mkName ":") `AppE` x'' `AppE` xs'
    mkBod ((i, MultiPiece _):_) = do
        let x' = VarE $ mkName $ "var" ++ show i
        tmp <- [|toMultiPiece|]
        return $ tmp `AppE` x'

-- | Whether the set of resources cover all possible URLs.
areResourcesComplete :: [THResource] -> Bool
areResourcesComplete res =
    let (slurps, noSlurps) = partitionEithers $ mapMaybe go res
     in case slurps of
            [] -> False
            _ -> let minSlurp = minimum slurps
                  in helper minSlurp $ reverse $ sort noSlurps
  where
    go :: THResource -> Maybe (Either Int Int)
    go (_, Simple ps _) =
        case reverse ps of
            [] -> Just $ Right 0
            (MultiPiece _:rest) -> go' Left rest
            x -> go' Right x
    go (n, SubSite{ssPieces = ps}) =
        go (n, Simple (ps ++ [MultiPiece ""]) [])
    go' b x = if all isSingle x then Just (b $ length x) else Nothing
    helper 0 _ = True
    helper _ [] = False
    helper m (i:is)
        | i >= m = helper m is
        | i + 1 == m = helper i is
        | otherwise = False
    isSingle (SinglePiece _) = True
    isSingle _ = False

notStatic :: Piece -> Bool
notStatic StaticPiece{} = False
notStatic _ = True

createDispatch :: Exp -- ^ modify a master handler
               -> Exp -- ^ convert a subsite handler to a master handler
               -> [THResource]
               -> Q [Clause]
createDispatch modMaster toMaster = mapM go
  where
    go (n, Simple ps methods) = do
        meth <- newName "method"
        xs <- mapM newName $ replicate (length $ filter notStatic ps) "x"
        let pat = [ ConP (mkName n) $ map VarP xs
                  , if null methods then WildP else VarP meth
                  ]
        bod <- go' n meth xs methods
        return $ Clause pat (NormalB bod) []
    go (n, SubSite{ssDispatch = d, ssToMasterArg = tma, ssPieces = ps}) = do
        meth <- newName "method"
        x <- newName "x"
        xs <- mapM newName $ replicate (length $ filter notStatic ps) "x"
        let pat = [ConP (mkName n) $ map VarP xs ++ [VarP x], VarP meth]
        let bod = d `AppE` VarE x `AppE` VarE meth
        fmap' <- [|fmap|]
        let routeToMaster = foldl AppE (ConE (mkName n)) $ map VarE xs
            tma' = foldl AppE tma $ map VarE xs
        let toMaster' = toMaster `AppE` routeToMaster `AppE` tma' `AppE` VarE x
        let bod' = InfixE (Just toMaster') fmap' (Just bod)
        let bod'' = InfixE (Just modMaster) fmap' (Just bod')
        return $ Clause pat (NormalB bod'') []
    go' n _ xs [] = do
        jus <- [|Just|]
        let bod = foldl AppE (VarE $ mkName $ "handle" ++ n) $ map VarE xs
        return $ jus `AppE` (modMaster `AppE` bod)
    go' n meth xs methods = do
        noth <- [|Nothing|]
        j <- [|Just|]
        let noMatch = Match WildP (NormalB noth) []
        return $ CaseE (VarE meth) $ map (go'' n xs j) methods ++ [noMatch]
    go'' n xs j method =
        let pat = LitP $ StringL method
            func = map toLower method ++ n
            bod = foldl AppE (VarE $ mkName func) $ map VarE xs
         in Match pat (NormalB $ j `AppE` (modMaster `AppE` bod)) []
