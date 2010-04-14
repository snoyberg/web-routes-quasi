module Static where

import Network.Wai
import Network.Wai.Enumerator
import Data.ByteString.Lazy.Char8 (pack)
import Web.Routes
import Web.Encodings
import System.Directory

data StaticRoutes = StaticRoutes { unStaticRoutes :: [String] }
    deriving (Show, Read, Eq)

siteStatic :: Site StaticRoutes (String -> Application -> FilePath -> Application)
siteStatic = Site
    { handleSite = \_ (StaticRoutes r) method badMethod fp -> serveFile method badMethod r fp
    , formatPathSegments = unStaticRoutes
    , parsePathSegments = Right . StaticRoutes
    }

serveFile :: String -> Application -> [String] -> FilePath -> Application
serveFile "GET" _ s fp _ = do
    let fp' = fp ++ concatMap ((:) '/') s
    e <- doesFileExist fp'
    if e
        then return Response
                { status = Status200
                , responseHeaders = [] -- FIXME content-type
                , responseBody = Left fp'
                }
        else return Response
                { status = Status404
                , responseHeaders = []
                , responseBody = Right $ fromLBS $ pack "Not found"
                }
serveFile _ badMethod _ _ req = badMethod req
