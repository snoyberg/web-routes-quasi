{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Blog where

import Web.Routes.Quasi
import Network.Wai
import Network.Wai.Enumerator
import Static
import Language.Haskell.TH.Syntax
import Data.String

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L

newtype Slug = Slug { unSlug :: String }
    deriving (Eq, Read, Show, SinglePiece)
instance IsString Slug where
    fromString = Slug

data Entry = Entry
    { entrySlug :: Slug
    , entryTitle :: String
    , entryContent :: String
    }

data BlogArgs = BlogArgs
    { staticPath :: Static
    , blogTitle :: String
    , blogEntries :: [Entry]
    }

newtype MyApp arg url = MyApp
    { runMyApp :: (url -> String)
               -> url
               -> (url -> url)
               -> arg
               -> (arg -> arg)
               -> Application
               -> String
               -> Application
    }

newtype MyInt = MyInt Int
    deriving (Num, Integral, Eq, Show, Real, Enum, Ord, Read, SinglePiece)

newtype MySlurp = MySlurp { unMySlurp :: [String] }
    deriving (Eq, Show, Read, MultiPiece)

createQuasiSite' QuasiSiteSettings
    { crRoutes = mkName "BlogRoutes"
    , crApplication = ConT ''Application
    , crArgument = ConT ''BlogArgs
    , crExplode = VarE $ mkName "runMyApp"
    , crResources = [$parseRoutes|
/                Home       GET
/entry/#Slug     EntryRoute GET
/fake/#MyInt     Fake
/slurp/*MySlurp  Slurp      GET
/static          StaticR    Static siteStatic staticPath
|]
    , crSite = mkName "siteBlog"
    , crMaster = Left $ ConT ''BlogArgs
    }

handleFake :: MyInt -> MyApp BlogArgs BlogRoutes
handleFake = undefined

getSlurp :: MySlurp -> MyApp BlogArgs BlogRoutes
getSlurp = undefined

getHome :: MyApp BlogArgs BlogRoutes
getHome = MyApp $ \f _ _ ba _ _ _ _-> return Response
    { status = Status302
    , responseHeaders = [(Location, S.pack $ f $ EntryRoute $ entrySlug
                                           $ head $ blogEntries ba)]
    , responseBody = Right $ fromLBS $ L.pack ""
    }

getEntryRoute :: Slug -> MyApp BlogArgs BlogRoutes
getEntryRoute slug = MyApp $ \f _ _ ba _ _ _ _ ->
    case filter (\x -> entrySlug x == slug) $ blogEntries ba of
        [] -> return Response
                { status = Status404
                , responseHeaders = []
                , responseBody = Right $ fromLBS $ L.pack "Not found"
                }
        (e:_) -> return Response
                { status = Status200
                , responseHeaders = [(ContentType, S.pack "text/html")]
                , responseBody = Right $ fromLBS $ L.pack $ concat
                    [ "<!DOCTYPE html>\n<html><head><title>"
                    , blogTitle ba
                    , "</title><link rel=\"stylesheet\" href=\""
                    , f $ StaticR $ StaticRoutes ["style.css"]
                    , "\"></head><body><ul>"
                    , concatMap (\e' -> concat
                        [ "<li><a href=\""
                        , f $ EntryRoute $ entrySlug e'
                        , "\">"
                        , entryTitle e'
                        , "</a></li>"
                        ]) $ blogEntries ba
                    , "</ul><h1>"
                    , entryTitle e
                    , "</h1><p>"
                    , entryContent e
                    , "</p></body></html>"
                    ]
                }
