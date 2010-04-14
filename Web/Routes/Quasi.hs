{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Web.Routes.Quasi
    ( -- * Data types
      Resource (..)
    , Handler (..)
    , Piece (..)
      -- * Quasi quoter
    , parseRoutes
      -- * Template haskell
    , createRoutes
    ) where

import Data.Char
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Data.Data
import Data.Maybe
import Control.Monad
import Web.Routes.Site

-- | A single resource pattern.
--
-- First argument is the name of the constructor, second is that URL pattern to
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
-- of the datatype for the routes. The second is a function returning a 'Site'
-- for that type of routes.
data Handler = ByMethod [(String, String)] -- ^ (method, handler)
             | Single String
             | SubSite String String String
    deriving (Read, Show, Eq, Data, Typeable)

-- | A single piece of a URL, delimited by slashes.
--
-- In the case of StaticPiece, the argument is the value of the piece; for the
-- other constructors, it is the name of the parameter represented by this
-- piece. That value is not used here, but may be useful elsewhere.
data Piece = StaticPiece String
           | StringPiece String
           | IntPiece String
           | SlurpPiece String
    deriving (Read, Show, Eq, Data, Typeable)

isStatic :: Piece -> Bool
isStatic (StaticPiece _) = True
isStatic _ = False

isSubSite :: Handler -> Bool
isSubSite (SubSite _ _ _) = True
isSubSite _ = False

-- | Drop leading whitespace.
trim :: String -> String
trim = dropWhile isSpace

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
                 in if all isStatic pieces || not (isSubSite handler)
                        then Resource constr pieces handler
                        else error "Subsites must have static pieces"
            _ -> error $ "Invalid resource line: " ++ s
    go' constr [] = Single $ "handle" ++ constr
    go' _ [routes, getSite@(x:_), grabArgs@(y:_)]
        | isLower x && isLower y = SubSite routes getSite grabArgs
    go' constr rest = ByMethod
                      $ map (\x -> (x, map toLower x ++ constr)) rest

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

-- | A quasi-quoter to parse a string into a list of 'Resource's.
parseRoutes :: QuasiQuoter
parseRoutes = QuasiQuoter x y where
    x = liftResources . resourcesFromString
    y = dataToPatQ (const Nothing) . resourcesFromString

liftResources :: [Resource] -> Q Exp
liftResources = fmap ListE . mapM go where
    go :: Resource -> Q Exp
    go (Resource s ps h) = do
        r <- [|Resource|]
        s' <- lift s
        ps' <- liftPieces ps
        h' <- liftHandler h
        return $ r `AppE` s' `AppE` ps' `AppE` h'

liftPieces :: [Piece] -> Q Exp
liftPieces = fmap ListE . mapM go where
    go (StaticPiece s) = do
        c <- [|StaticPiece|]
        s' <- lift s
        return $ c `AppE` s'
    go (StringPiece s) = do
        c <- [|StringPiece|]
        s' <- lift s
        return $ c `AppE` s'
    go (IntPiece s) = do
        c <- [|IntPiece|]
        s' <- lift s
        return $ c `AppE` s'
    go (SlurpPiece s) = do
        c <- [|SlurpPiece|]
        s' <- lift s
        return $ c `AppE` s'

liftHandler :: Handler -> Q Exp
liftHandler (ByMethod s) = do
    c <- [|ByMethod|]
    s' <- lift s
    return $ c `AppE` s'
liftHandler (Single s) = do
    c <- [|Single|]
    s' <- lift s
    return $ c `AppE` s'
liftHandler (SubSite x y z) = do
    c <- [|SubSite|]
    x' <- lift x
    y' <- lift y
    z' <- lift z
    return $ c `AppE` x' `AppE` y' `AppE` z'

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
    go'' (SubSite t _ _) = [(NotStrict, ConT $ mkName t)]
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
                    SubSite _ f _ -> do
                        parse <- [|parsePathSegments|]
                        let parse' = parse `AppE` VarE (mkName f)
                        let rhs = parse' `AppE` VarE (mkName "var0")
                        fm <- [|fmapEither|]
                        return $ fm `AppE` bod `AppE` rhs
                    _ -> do
                        ri <- [|Right|]
                        return $ AppE ri bod
        checkInts' <- checkInts ps'
        return $ Clause [pat]
                        (GuardedB [(NormalG checkInts', bod')]) []
    mkPat [] (SubSite _ _ _) = VarP $ mkName "var0"
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
        re <- [|read|]
        return $ x `AppE` (re `AppE` VarE (mkName $ "var" ++ show i))
    checkInts [] = [|True|]
    checkInts ((i, IntPiece _):rest) = do
        ii <- [|isInt|]
        a <- [|(&&)|]
        rest' <- checkInts rest
        return $ a `AppE` (ii `AppE` VarE (mkName $ "var" ++ show i))
                   `AppE` rest'
    checkInts (_:rest) = checkInts rest

isInt :: String -> Bool
isInt [] = False
isInt ('-':rest) = all isDigit rest
isInt x = all isDigit x

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
    lastPat (SubSite _ _ _) = [VarP $ mkName "var0"]
    lastPat _ = []
    go' (_, StaticPiece _) = Nothing
    go' (i, _) = Just $ VarP $ mkName $ "var" ++ show (i :: Int)
    mkBod [] (SubSite _ f _) = do
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
    let url = ConT $ mkName s
        str = ConT ''String
        rend = ArrowT `AppT` url `AppT` str
        ret1 = ArrowT `AppT` ConT p `AppT` ConT a -- param -> app
        ret2 = ArrowT `AppT` ConT a `AppT` ret1 -- app -> ret1
        ret3 = ArrowT `AppT` str `AppT` ret2 -- String -> ret2 (method)
        ret4 = ArrowT `AppT` url `AppT` ret3 -- url -> ret3
        ret5 = ArrowT `AppT` rend `AppT` ret4 -- (url -> String) -> ret4
    return $ SigD (mkName $ "dispatch" ++ s) ret5

dispDec :: String -> [Resource] -> String -> Q Dec
dispDec s r explode = do
    -- (url -> String) -> url -> String -> app -> param -> app
    badMethod <- newName "_badMethod"
    param <- newName "_param"
    method <- newName "_method"
    render <- newName "render"
    clauses <- mapM (go badMethod param method render) r
    return $ FunD (mkName $ "dispatch" ++ s) $ clauses
  where
    go badMethod param method render (Resource constr ps handler) = do
        conArgs <- go' ps handler
        let pat = [ VarP render
                  , ConP (mkName constr) $ map VarP conArgs
                  , VarP method
                  , VarP badMethod
                  , VarP param
                  ]
        b <- case handler of
                Single s' -> do
                    unexploded <- foldM go'' (VarE $ mkName s') conArgs
                    let exploded = VarE (mkName explode) `AppE` unexploded
                    return $ exploded `AppE` VarE param `AppE` VarE render
                ByMethod methods -> do
                    matches <- forM methods $ \(m, f) -> do
                        let pat' = LitP $ StringL m
                        unexploded <- foldM go'' (VarE $ mkName f) conArgs
                        let exploded = VarE (mkName explode) `AppE` unexploded
                        let bod = exploded `AppE` VarE param
                                           `AppE` VarE render
                        return $ Match pat' (NormalB bod) []
                    let final =
                            if length methods == 4
                                then []
                                else [Match WildP (NormalB $ VarE badMethod) []]
                    return $ CaseE (VarE method) $ matches ++ final
                SubSite _ f getArg -> do
                    hs <- [|handleSite|]
                    o <- [|(.)|]
                    let render' = o `AppE` VarE render `AppE` ConE (mkName constr)
                    let disp = hs `AppE` VarE (mkName f)
                        disp1 = disp `AppE` render'
                        disp2 = disp1 `AppE` VarE (last conArgs)
                        disp3 = disp2 `AppE` VarE method
                        disp4 = disp3 `AppE` VarE badMethod
                        disp5 = disp4 `AppE` (VarE (mkName getArg) `AppE` VarE param)
                    return disp5
        return $ Clause pat (NormalB b) []
    go' [] (SubSite _ _ _) = do
        n <- newName "arg"
        return [n]
    go' [] _ = return []
    go' (StaticPiece _:rest) h = go' rest h
    go' (_:rest) h = do
        n <- newName "arg"
        ns <- go' rest h
        return $ n : ns
    go'' base arg = return $ base `AppE` VarE arg

siteDecType :: String -> Name -> Name -> Q Dec
siteDecType s a p = do
    let a1 = ArrowT `AppT` ConT p `AppT` ConT a -- args -> application
        a2 = ArrowT `AppT` ConT a `AppT` a1 -- applications -> a1
        a3 = ArrowT `AppT` ConT ''String `AppT` a2 -- String -> a2 (method)
        ret = ConT ''Site `AppT` ConT (mkName s) `AppT` a3
    return $ SigD (mkName $ "site" ++ s) ret

siteDec :: String -> Q Dec
siteDec s = do
    -- Site routes (((Method -> app) -> app) -> app -> param -> app)
    body <- go
    return $ FunD (mkName $ "site" ++ s)
        [ Clause [] (NormalB body) []
        ]
  where
    go = do
        {-
        m <- newName "getMethod"
        bm <- newName "badMethod"
        p <- newName "param"
        -}
        let hs = VarE (mkName $ "dispatch" ++ s)
        {-
                    `AppE` VarE bm
                    `AppE` VarE p
        gm <- [|grabMethod|]
        let hs' = gm `AppE` VarE m `AppE` hs
        -}
        let hs' = hs
        let fps = VarE (mkName $ "render" ++ s)
        let pps = VarE (mkName $ "parse" ++ s)
        si <- [|Site|]
        return $ si `AppE` hs' `AppE` fps `AppE` pps

-- | Template haskell code to convert a list of 'Resource's into appropriate
-- declarations.
--
-- This function takes four arguments in addition to the list of resources.
--
-- * The first is the name of the data type for routes; this function will
-- declare that data type with constructors according to the resources.
--
-- * The second is the datatype of an application. This depends on your
-- underlying web server; in the case of WAI, you would use
-- Network.Wai.Application. This is the value ultimately returned by the
-- dispatch function, and must be the output type of the fourth argument.
--
-- * The third argument is the data type for arguments. This is a data type
-- which you must define, and which will be available to all handler functions.
--
-- * The fourth is the trickiest; it is an explode function, designed to make
-- writing of handler functions much simpler. It is a function with type
-- signature:
--     \"myapp -> args -> (url -> String) -> application\"
--  where args, url and application are the 3rd, 1st and 2nd arguments,
--  respectively.
--
--  This function produces 5 declarations (plus type signatures). For
--  simplicity's sake, let's assume createRoutes was called as follows:
--
--  > createRoutes \"MyRoutes\" ''Application ''MyArgs \"myExplode\" resources
--
--  With:
--
--  > myExplode :: MyApp url -> MyArgs -> (url -> String) -> Application
--
--  * Defines the routes data type.
--
--  * parseMyRoutes :: [String] -> Either String MyRoutes
--
--  * renderMyRoutes :: MyRoutes -> [String]
--
--  * dispatchMyRoutes :: (MyRoutes -> String) -> BlogRoutes -> String ->
--  Application -> MyArgs -> Application. In this signature, the first argument
--  is a handler for unsupported methods, and the third is the requested
--  method.
--
--  * siteMyRoutes :: Site MyRoutes (String -> Application -> MyArgs ->
--  Application). The first argument is the method and the second handles
--  unsupported methods.
createRoutes :: String -- ^ name for routes data type
             -> Name -- ^ type for application
             -> Name -- ^ data type for arguments
             -> String -- ^ explode function; converts to application
             -> [Resource]
             -> Q [Dec]
createRoutes name app param explode res = do
    dt <- dataTypeDec name res
    pat <- parseDecType name
    pa <- parseDec name res
    ret <- renderDecType name
    re <- renderDec name res
    dit <- dispDecType name app param
    di <- dispDec name res explode
    st <- siteDecType name app param
    s <- siteDec name
    return [dt, pat, pa, ret, re, dit, di, st, s]
