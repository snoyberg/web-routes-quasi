{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Blog where

import Web.Routes.Quasi
import Web.Routes.Site
import Network.Wai
import Network.Wai.Enumerator
import Static
import Language.Haskell.TH.Syntax

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L

data Entry = Entry
    { entrySlug :: String
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

createQuasiSite' QuasiSiteSettings
    { crRoutes = mkName "BlogRoutes"
    , crApplication = ConT ''Application
    , crArgument = ConT ''BlogArgs
    , crExplode = VarE $ mkName "runMyApp"
    , crResources = [$parseRoutes|
/                Home       GET
/entry/$         EntryRoute GET
/fake/#          Fake
/static          StaticR    Static siteStatic staticPath
|]
    , crSite = mkName "siteBlog"
    , crMaster = Left $ ConT ''BlogArgs
    }

handleFake :: Integer -> MyApp BlogArgs BlogRoutes
handleFake = undefined

getHome :: MyApp BlogArgs BlogRoutes
getHome = MyApp $ \f _ _ ba _ _ _ _-> return Response
    { status = Status302
    , responseHeaders = [(Location, S.pack $ f $ EntryRoute $ entrySlug
                                           $ head $ blogEntries ba)]
    , responseBody = Right $ fromLBS $ L.pack ""
    }

getEntryRoute :: String -> MyApp BlogArgs BlogRoutes
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
