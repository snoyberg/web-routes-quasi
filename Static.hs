module Static where

import Network.Wai
import Network.Wai.Enumerator
import Data.ByteString.Lazy.Char8 (pack)
import Web.Routes
import Web.Encodings
import System.Directory

data StaticRoutes = StaticRoutes { unStaticRoutes :: [String] }
    deriving (Show, Read, Eq)

siteStatic :: FilePath -> Site StaticRoutes Application
siteStatic fp = Site
    { handleSite = \_ (StaticRoutes r) _ -> serveFile fp r
    , defaultPage = Nothing
    , formatPathSegments = unStaticRoutes
    , parsePathSegments = Right . StaticRoutes
    }

serveFile :: FilePath -> [String] -> IO Response
serveFile fp s = do
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
