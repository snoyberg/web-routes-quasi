{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module Web.Routes.Quasi
    (
      -- * Quasi quoter
      parseRoutes
    , parseRoutesNoCheck
      -- * Template haskell
    , createParse
    , createRender
    , createQuasiDispatch
    , createRoutes
    , createRoutes'
    , CreateRoutesSettings (..)
    , CreateRoutesResult (..)
      -- * Quasi site
    , QuasiDispatch
    , QuasiSite (..)
    , quasiFromSite
    , quasiToSite
      -- * Underlying data types
    , Resource (..)
    , Handler (..)
    , Piece (..)
#if TEST
    , testSuite
#endif
    ) where

import Data.Char
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Data.Data
import Data.Maybe
import Control.Monad
import Web.Routes.Site
import Data.Either
import Data.List

#if TEST
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
#endif

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

type QuasiDispatch app surl sarg murl marg
                   = (murl -> String)
                  -> surl
                  -> (surl -> murl)
                  -> marg
                  -> (marg -> sarg)
                  -> app -- ^ bad method handler
                  -> String -- ^ method
                  -> app

data QuasiSite app surl sarg murl marg = QuasiSite
    { quasiDispatch :: QuasiDispatch app surl sarg murl marg
    , quasiRender :: surl -> [String]
    , quasiParse :: [String] -> Either String surl
    }

quasiFromSite :: Site surl app -> QuasiSite app surl () murl marg
quasiFromSite (Site dispatch render parse) = QuasiSite
    { quasiDispatch = \mrender surl constr _ _ _ _ ->
                        dispatch (mrender . constr) surl
    , quasiRender = render
    , quasiParse = parse
    }

quasiToSite :: QuasiSite app surl sarg surl sarg
            -> ((String -> app) -> app) -- ^ grab method
            -> app -- ^ bad method
            -> sarg
            -> Site surl app
quasiToSite (QuasiSite dispatch render parse) grabMethod badMethod sarg = Site
    { handleSite = \rend surl -> grabMethod (dispatch
                                    rend
                                    surl
                                    id
                                    sarg
                                    id
                                    badMethod)
    , formatPathSegments = render
    , parsePathSegments = parse
    }

isStatic :: Piece -> Bool
isStatic (StaticPiece _) = True
isStatic _ = False

isSubSite :: Handler -> Bool
isSubSite (SubSite _ _ _) = True
isSubSite _ = False

isString :: Piece -> Bool
isString (StringPiece _) = True
isString _ = False

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

-- | A quasi-quoter to parse a string into a list of 'Resource's. Checks for
-- overlapping routes, failing if present; use 'parseRoutesNoCheck' to skip the
-- checking.
parseRoutes :: QuasiQuoter
parseRoutes = QuasiQuoter x y where
    x s = do
        let res = resourcesFromString s
        case findOverlaps res of
            [] -> liftResources res
            _ -> error $ "Overlapping routes: " ++ show res
    y = dataToPatQ (const Nothing) . resourcesFromString

parseRoutesNoCheck :: QuasiQuoter
parseRoutesNoCheck = QuasiQuoter x y where
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

dataTypeDec :: CreateRoutesSettings -> Q Dec
dataTypeDec set =
    return $ DataD [] (crRoutes set) []
             (map go $ crResources set) claz
  where
    go (Resource n pieces h) = NormalC (mkName n)
                             $ mapMaybe go' pieces
                            ++ go'' h
    go' (StringPiece _) = Just (NotStrict, ConT ''String)
    go' (IntPiece _) = Just (NotStrict, ConT ''Integer)
    go' (SlurpPiece _) = Just (NotStrict, AppT ListT $ ConT ''String)
    go' _ = Nothing
    go'' (SubSite t _ _) = [(NotStrict, ConT $ mkName t)]
    go'' _ = []
    claz = [''Show, ''Read, ''Eq]

findOverlaps :: [Resource] -> [(Resource, Resource)]
findOverlaps = gos . map justPieces
  where
    justPieces r@(Resource _ ps (SubSite{})) = (ps ++ [SlurpPiece ""], r)
    justPieces r@(Resource _ ps _) = (ps, r)
    gos [] = []
    gos (x:xs) = mapMaybe (go x) xs ++ gos xs
    go (StaticPiece x:xs, xr) (StaticPiece y:ys, yr)
        | x == y = go (xs, xr) (ys, yr)
        | otherwise = Nothing
    go (SlurpPiece _:_, xr) (_, yr) = Just (xr, yr)
    go (_, xr) (SlurpPiece _:_, yr) = Just (xr, yr)
    go (StaticPiece x:xs, xr) (IntPiece _:ys, yr)
        | isInt x = go (xs, xr) (ys, yr)
        | otherwise = Nothing
    go (IntPiece _:xs, xr) (StaticPiece y:ys, yr)
        | isInt y = go (xs, xr) (ys, yr)
        | otherwise = Nothing
    go ([], xr) ([], yr) = Just (xr, yr)
    go ([], _) (_, _) = Nothing
    go (_, _) ([], _) = Nothing
    go (_:xs, xr) (_:ys, yr) = go (xs, xr) (ys, yr)

-- | Whether the set of resources cover all possible URLs.
areResourcesComplete :: [Resource] -> Bool
areResourcesComplete res =
    let (slurps, noSlurps) = partitionEithers $ mapMaybe go res
     in case slurps of
            [] -> False
            _ -> let minSlurp = minimum slurps
                  in helper minSlurp $ reverse $ sort noSlurps
  where
    go :: Resource -> Maybe (Either Int Int)
    go (Resource _ ps (SubSite _ _ _)) = go' Left ps
    go (Resource _ ps _) =
        case reverse ps of
            [] -> Just $ Right 0
            (SlurpPiece _:rest) -> go' Left rest
            x -> go' Right x
    go' b x = if all isString x then Just (b $ length x) else Nothing
    helper 0 _ = True
    helper _ [] = False
    helper m (i:is)
        | i >= m = helper m is
        | i + 1 == m = helper i is
        | otherwise = False

-- | Generates the set of clauses necesary to parse the given 'Resource's.
createParse :: [Resource] -> Q [Clause]
createParse res = do
    final' <- final
    clauses <- mapM go res
    return $ if areResourcesComplete res
                then clauses
                else clauses ++ [final']
  where
    final = do
        no <- [|Left "Invalid URL"|]
        return $ Clause [WildP] (NormalB no) []
    go (Resource n ps h) = do
        let ps' = zip [1..] ps
        let pat = mkPat ps' h
        bod <- foldM go' (ConE $ mkName n) ps'
        bod' <- case h of
                    SubSite _ f _ -> do
                        parse <- [|quasiParse|]
                        let parse' = parse `AppE` VarE (mkName f)
                        let rhs = parse' `AppE` VarE (mkName "var0")
                        fm <- [|fmap|]
                        return $ fm `AppE` bod `AppE` rhs
                    _ -> do
                        ri <- [|Right|]
                        return $ AppE ri bod
        checkInts' <- checkInts ps'
        return $ Clause [pat]
                        (GuardedB [(NormalG checkInts', bod')]) []
    mkPat [] (SubSite _ _ _) = VarP $ mkName "var0" -- FIXME use newName
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

-- | Generates the set of clauses necesary to render the given 'Resource's.
createRender :: [Resource] -> Q [Clause]
createRender res = mapM go res
  where
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
        format <- [|quasiRender|]
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

createQuasiDispatch :: CreateRoutesSettings -> Q [Clause]
createQuasiDispatch set = do
    mrender <- newName "_mrender"
    tomurl <- newName "_tomurl"
    marg <- newName "_marg"
    tosarg <- newName "_tosarg"
    method <- newName "_method"
    badMethod <- newName "_badMethod"
    mapM (go mrender tomurl marg tosarg method badMethod) $ crResources set
  where
    go mrender tomurl marg tosarg method badMethod
       (Resource constr ps handler) = do
        conArgs <- go' ps handler
        url <- newName "_url"
        let pat = [ VarP mrender
                  , AsP url $ ConP (mkName constr) $ map VarP conArgs
                  , VarP tomurl
                  , VarP marg
                  , VarP tosarg
                  , VarP badMethod
                  , VarP method
                  ]
        b <- case handler of
                Single s' -> do
                    unexploded <- foldM go'' (VarE $ mkName s') conArgs
                    let exploded = crExplode set `AppE` unexploded
                    return $ exploded
                                `AppE` VarE mrender
                                `AppE` VarE url
                                `AppE` VarE tomurl
                                `AppE` VarE marg
                                `AppE` VarE tosarg
                                `AppE` VarE badMethod
                                `AppE` VarE method
                ByMethod methods -> do
                    matches <- forM methods $ \(m, f) -> do
                        let pat' = LitP $ StringL m
                        unexploded <- foldM go'' (VarE $ mkName f) conArgs
                        let exploded = crExplode set `AppE` unexploded
                        let bod = exploded
                                `AppE` VarE mrender
                                `AppE` VarE url
                                `AppE` VarE tomurl
                                `AppE` VarE marg
                                `AppE` VarE tosarg
                                `AppE` VarE badMethod
                                `AppE` VarE method
                        return $ Match pat' (NormalB bod) []
                    let final =
                            if length methods == 4
                                then []
                                else [Match WildP (NormalB $ VarE badMethod) []]
                    return $ CaseE (VarE method) $ matches ++ final
                SubSite _ f getArg -> do
                    qd <- [|quasiDispatch|]
                    let disp = qd `AppE` VarE (mkName f)
                    o <- [|(.)|]
                    let tomurl' = InfixE (Just $ VarE tomurl) o
                                $ Just $ ConE $ mkName constr
                    let tosarg' = InfixE (Just $ VarE $ mkName getArg) o
                                $ Just $ VarE tosarg
                    return $ disp
                                `AppE` VarE mrender
                                `AppE` VarE (last conArgs)
                                `AppE` tomurl'
                                `AppE` VarE marg
                                `AppE` tosarg'
                                `AppE` VarE badMethod
                                `AppE` VarE method
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

siteDecType :: CreateRoutesSettings -> Q Dec
siteDecType set = do
    let marg = mkName "marg"
        murl = mkName "murl"
    return $ SigD (crSite set) $ ForallT
        [PlainTV marg, PlainTV murl]
        [] $ ConT ''QuasiSite
                `AppT` crApplication set
                `AppT` ConT (crRoutes set)
                `AppT` crArgument set
                `AppT` VarT murl
                `AppT` VarT marg

siteDec :: Name -- ^ name of resulting function
        -> [Clause] -- ^ parse
        -> [Clause] -- ^ render
        -> [Clause] -- ^ dispatch
        -> Q Dec
siteDec name parse render dispatch = do
    si <- [|QuasiSite|]
    dname <- newName "dispatch"
    rname <- newName "render"
    pname <- newName "parse"
    let body = si `AppE` VarE dname
                  `AppE` VarE rname
                  `AppE` VarE pname
    return $ FunD name
        [ Clause [] (NormalB body)
            [ FunD dname dispatch
            , FunD rname render
            , FunD pname parse
            ]
        ]

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
createRoutes :: CreateRoutesSettings -> Q CreateRoutesResult
createRoutes set = do
    dt <- dataTypeDec set
    parseClauses <- createParse $ crResources set
    renderClauses <- createRender $ crResources set
    dispatchClauses <- createQuasiDispatch set
    st <- siteDecType set
    s <- siteDec (crSite set) parseClauses renderClauses dispatchClauses
    return CreateRoutesResult
        { decRoutes = dt
        , decSiteType = st
        , decSite = s
        }

data CreateRoutesSettings = CreateRoutesSettings
    { crRoutes :: Name
    , crApplication :: Type
    , crArgument :: Type
    , crExplode :: Exp
    , crResources :: [Resource]
    , crSite :: Name
    }

data CreateRoutesResult = CreateRoutesResult
    { decRoutes :: Dec
    , decSiteType :: Dec
    , decSite :: Dec
    }

createRoutes' :: CreateRoutesSettings -> Q [Dec]
createRoutes' s = do
    CreateRoutesResult x y z <- createRoutes s
    return [x, y, z]

#if TEST
testSuite :: Test
testSuite = testGroup "Web.Routes.Quasi"
    [ testCase "overlaps" caseOverlaps
    , testCase "complete" caseComplete
    ]

caseOverlaps :: Assertion
caseOverlaps = do
    assertBool "empty" $ null $ findOverlaps []
    assertBool "single" $ null $ findOverlaps
                [ Resource "Foo" [] $ Single "foo"
                ]
    assertBool "two empties" $ not $ null $ findOverlaps
                [ Resource "Foo" [] $ Single "foo"
                , Resource "Bar" [] $ Single "bar"
                ]
    assertBool "slurp versus empty" $ not $ null $ findOverlaps
                [ Resource "Foo" [] $ Single "foo"
                , Resource "Bar" [] $ SubSite "a" "b" "c"
                ]
    assertBool "int + slurp versus empty" $ null $ findOverlaps
                [ Resource "Foo" [] $ Single "foo"
                , Resource "Bar" [IntPiece ""] $ SubSite "a" "b" "c"
                ]

caseComplete :: Assertion
caseComplete = do
    assertBool "empty" $ not $ areResourcesComplete []
    assertBool "slurp" $ areResourcesComplete
                [ Resource "Foo" [SlurpPiece "foo"] $ Single "foo"
                ]
    assertBool "subsite" $ areResourcesComplete
                [ Resource "Foo" [] $ SubSite "a" "b" "c"
                ]
    assertBool "string + subsite" $ areResourcesComplete
                [ Resource "Foo" [StringPiece "x"] $ SubSite "a" "b" "c"
                , Resource "Bar" [] $ Single "bar"
                ]
    assertBool "int + subsite" $ not $ areResourcesComplete
                [ Resource "Foo" [IntPiece "x"] $ SubSite "a" "b" "c"
                ]
    assertBool "two pieces" $ not $ areResourcesComplete
                [ Resource "Foo" [StringPiece "x"] $ Single "foo"
                , Resource "Bar" [IntPiece "x"] $ SubSite "a" "b" "c"
                ]
#endif
