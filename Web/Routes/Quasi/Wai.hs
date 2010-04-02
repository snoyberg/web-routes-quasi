{-# LANGUAGE TemplateHaskell #-}
module Web.Routes.Quasi.Wai where

import Network.Wai
import Network.Wai.Enumerator
import Web.Routes
import qualified Web.Routes.Quasi as Q

import Data.ByteString.Char8 (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as L
import Language.Haskell.TH.Syntax

drop1Slash :: String -> String
drop1Slash ('/':x) = x
drop1Slash x = x

data WaiArgs = WaiArgs
    { waiApproot :: String -- ^ include trailing slash
    , waiBadMethod :: Application
    , wai404 :: Application
    }

waiSite :: (((Q.Method -> Application) -> Application) -> Application -> args -> Site url Application)
        -> WaiArgs
        -> args
        -> Application
waiSite s wa args req = do
    let site = s (grabMethod wa) (waiBadMethod wa) args
    let pieces = filter (not . null)
               $ decodePathInfo $ drop1Slash $ unpack $ pathInfo req
    print ("pieces", pieces)
    case parsePathSegments site pieces of
        Left _ -> wai404 wa req
        Right url ->
            let format u = waiApproot wa ++ encodePathInfo
                            (formatPathSegments site u)
             in handleSite site format url req

grabMethod :: WaiArgs -> (Q.Method -> Application) -> Application
grabMethod wa f req =
    case readMethod $ requestMethod req of
        Nothing -> waiBadMethod wa req
        Just m -> f m req

readMethod :: Method -> Maybe Q.Method
readMethod m
    | m == GET    = Just Q.GET
    | m == POST   = Just Q.POST
    | m == PUT    = Just Q.PUT
    | m == DELETE = Just Q.DELETE
    | otherwise   = Nothing

defBadMethod :: Application
defBadMethod _ = return $ Response
    { status = Status 405 $ pack "Method Not Allowed"
    , responseHeaders = []
    , responseBody = Right $ fromLBS $ L.pack "Method not allowed"
    }

def404 :: Application
def404 _ = return $ Response
    { status = Status404
    , responseHeaders = []
    , responseBody = Right $ fromLBS $ L.pack "Not found"
    }

createWaiRoutes s =
    Q.createRoutes (s ++ "Routes") ''Application $ mkName $ s ++ "Args"
