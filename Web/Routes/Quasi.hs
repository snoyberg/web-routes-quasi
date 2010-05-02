{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
module Web.Routes.Quasi
    (
      -- * Quasi quoter
      parseRoutes
    , parseRoutesNoCheck
      -- * Template haskell
      -- ** Low level
    , createQuasiDispatch
    , createRender
    , createParse
      -- ** High level for 'QuasiSite's
    , createQuasiSite
    , QuasiSiteSettings (..)
    , QuasiSiteDecs (..)
      -- * Quasi site
    , QuasiDispatch
    , QuasiSite (..)
    , quasiFromSite
    , quasiToSite
    , Routes
    , BlankArgs (..)
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
-- of the datatype for the routes. The second is a function returning a
-- 'QuasiSite' for that type of routes. The third is a function converting the
-- master argument to the subsite argument.
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

type family Routes a

-- | The type for quasiDispatch; separated out for clarity of Haddock docs.
type QuasiDispatch app sub master
                   = (Routes master -> String)
                  -> Routes sub
                  -> (Routes sub -> Routes master)
                  -> master
                  -> (master -> sub)
                  -> app -- ^ bad method handler
                  -> String -- ^ method
                  -> app

-- | Very similar in principle to 'Site', but with special support for
-- arguments and subsites.
data QuasiSite app sub master = QuasiSite
    { quasiDispatch :: QuasiDispatch app sub master
    , quasiRender :: Routes sub -> [String]
    , quasiParse :: [String] -> Either String (Routes sub)
    }

-- | Used for applications with no arguments. In particular, this facilitates a
-- translation from a 'Site' to a 'QuasiSite' via 'quasiFromSite'.
data BlankArgs routes = BlankArgs
type instance Routes (BlankArgs routes) = routes

-- | Convert a 'Site' to a 'QuasiSite'. 'quasiRender' and 'quasiParse' are
-- identical to 'formatPathSegments' and 'parsePathSegments'; for the
-- 'quasiDispatch' function, we just ignore the extra arguments that 'Site'
-- does not use.
quasiFromSite :: Site surl app -> QuasiSite app (BlankArgs surl) master
quasiFromSite (Site dispatch render parse) = QuasiSite
    { quasiDispatch = \mrender surl constr _ _ _ _ ->
                        dispatch (mrender . constr) surl
    , quasiRender = render
    , quasiParse = parse
    }

-- | Convert a 'QuasiSite' to a 'Site'. 'quasiRender' and 'quasiParse' are
-- identical to 'formatPathSegments' and 'parsePathSegments'; for the
-- 'handleSite' function, we need some extra information passed to this
-- function. We also restrict the resulting 'QuasiSite' to cases where subsite
-- and master site are the same.
quasiToSite :: QuasiSite app sub sub
            -> ((String -> app) -> app) -- ^ grab method
            -> app -- ^ bad method
            -> sub
            -> Site (Routes sub) app
quasiToSite (QuasiSite dispatch render parse) grabMethod badMethod sub = Site
    { handleSite = \rend url -> grabMethod (dispatch
                                    rend
                                    url
                                    id
                                    sub
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
-- checking. See documentation site for details on syntax.
parseRoutes :: QuasiQuoter
parseRoutes = QuasiQuoter x y where
    x s = do
        let res = resourcesFromString s
        case findOverlaps res of
            [] -> liftResources res
            _ -> error $ "Overlapping routes: " ++ show res
    y = dataToPatQ (const Nothing) . resourcesFromString

-- | Same as 'parseRoutes', but performs no overlap checking.
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

dataTypeDec :: QuasiSiteSettings -> Q Dec
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
    go'' (SubSite t _ _) = [(NotStrict, ConT ''Routes `AppT` ConT (mkName t))]
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

-- | Generates the set of clauses necesary to parse the given 'Resource's. See 'quasiParse'.
createParse :: QuasiSiteSettings -> [Resource] -> Q [Clause]
createParse set res = do
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
                    SubSite argType f _ -> do
                        parse <- [|quasiParse|]
                        let siteType = ConT ''QuasiSite
                                        `AppT` crApplication set
                                        `AppT` ConT (mkName argType)
                                        `AppT` crArgument set
                            siteVar = VarE (mkName f) `SigE` siteType
                        let parse' = parse `AppE` siteVar
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

-- | Generates the set of clauses necesary to render the given 'Resource's. See
-- 'quasiRender'.
createRender :: QuasiSiteSettings -> [Resource] -> Q [Clause]
createRender set res = mapM go res
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
    mkBod [] (SubSite argType f _) = do
        format <- [|quasiRender|]
        let siteType = ConT ''QuasiSite
                        `AppT` crApplication set
                        `AppT` ConT (mkName argType)
                        `AppT` crArgument set
            siteVar = VarE (mkName f) `SigE` siteType
        let format' = format `AppE` siteVar
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

-- | Generate the set of clauses necesary to dispatch the given 'Resource's.
-- See 'quasiDispatch'.
createQuasiDispatch :: QuasiSiteSettings -> Q [Clause]
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
                SubSite argType f getArg -> do
                    qd <- [|quasiDispatch|]
                    let siteType = ConT ''QuasiSite
                                    `AppT` crApplication set
                                    `AppT` ConT (mkName argType)
                                    `AppT` crArgument set
                        siteVar = VarE (mkName f) `SigE` siteType
                    let disp = qd `AppE` siteVar
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

siteDecType :: QuasiSiteSettings -> Q Dec
siteDecType set = do
    let core = ConT ''QuasiSite `AppT` crApplication set `AppT` crArgument set
    let ty = case crMaster set of
                Left master -> core `AppT` master
                Right classes ->
                    let master = mkName "master"
                        master' = VarT master
                        cxt = map (flip ClassP [master']) classes
                     in ForallT [PlainTV master] cxt $ core `AppT` master'
    return $ SigD (crSite set) ty

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
-- declarations for a 'QuasiSite'. See the 'QuasiSiteSettings' and
-- 'QuasiSiteDecs' data types for an explanation for the input and output,
-- respectively, of this function.
createQuasiSite :: QuasiSiteSettings -> Q QuasiSiteDecs
createQuasiSite set = do
    dt <- dataTypeDec set
    let tySyn = TySynInstD ''Routes [crArgument set] $ ConT $ crRoutes set
    parseClauses <- createParse set $ crResources set
    renderClauses <- createRender set $ crResources set
    dispatchClauses <- createQuasiDispatch set
    st <- siteDecType set
    s <- siteDec (crSite set) parseClauses renderClauses dispatchClauses
    return QuasiSiteDecs
        { decRoutes = dt
        , decRoutesSyn = tySyn
        , decSiteType = st
        , decSite = s
        }

-- | The arguments passed to 'createQuasiSite' for generating applications
-- based on the 'QuasiSite' datatype.
data QuasiSiteSettings = QuasiSiteSettings
    { -- | The name for the URL data type to be created.
      crRoutes :: Name
      -- | The type for underlying applications.
    , crApplication :: Type
      -- | The type for the argument value to be passed to dispatch functions.
    , crArgument :: Type
      -- | Underlying applications will often want to program against some
      -- datatype. The explode function converts that datatype into a function
      -- that will generate an application ('crApplication'). In particular,
      -- the value of crExplode should have a type signature of:
      --
      -- > explode :: handler
      -- >         -> ('Routes' master -> String)
      -- >         -> 'Routes' sub
      -- >         -> ('Routes' sub -> 'Routes' master)
      -- >         -> master
      -- >         -> (master -> sub)
      -- >         -> app
      -- >         -> String
      -- >         -> app
      --
      -- handler is some datatype handled by the calling application;
      -- web-routes-quasi needn't know about it. sub and master are the
      -- arguments for the subsite and master site, respectively. app is the
      -- datatype for the underlying application; the app argument above is the
      -- handler for unsupported method. The 'String' argument is the request
      -- method.
    , crExplode :: Exp
      -- | The 'Resource's upon which we are building the set of URLs and
      -- dispatches. Usually generated by 'parseRoutes'.
    , crResources :: [Resource]
      -- | The name for the resulting function which will return the 'QuasiSite'.
    , crSite :: Name
      -- | Describes the type of the master argument. This can either be a
      -- 'Left' concrete datatype, or 'Right' a list of 'Pred's describing the
      -- context for master.
    , crMaster :: Either Type [Name]
    }

-- | The template Haskell declarations returned from 'createQuasiSite'.
data QuasiSiteDecs = QuasiSiteDecs
    { -- | Defines the actual URL datatype, which all its constructors.
      decRoutes :: Dec
      -- | Defines the 'Routes' type synonym instance between the argument
      -- ('crArgument') and URL datatype.
    , decRoutesSyn :: Dec
      -- | The type signature for the site function ('decSite').
    , decSiteType :: Dec
      -- | Function which returns a 'QuasiSite'. The type parameters for the
      -- 'QuasiSite' will be 'crApplication', 'crArgument' and a forall master.
    , decSite :: Dec
    }

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
