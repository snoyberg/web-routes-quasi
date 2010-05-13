{-# LANGUAGE TypeFamilies #-}
module Static where

import Network.Wai
import Network.Wai.Enumerator
import Data.ByteString.Lazy.Char8 (pack)
import System.Directory
import Web.Routes.Quasi

data Static = Static FilePath
type instance Routes Static = StaticRoutes

data StaticRoutes = StaticRoutes { unStaticRoutes :: [String] }
    deriving (Show, Read, Eq)

siteStatic :: QuasiSite Application Static master
siteStatic = QuasiSite
    { quasiDispatch = \_ (StaticRoutes r) _ _ _ badMethod method -> serveFile method badMethod r "static" -- FIXME
    , quasiRender = unStaticRoutes
    , quasiParse = Right . StaticRoutes
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
