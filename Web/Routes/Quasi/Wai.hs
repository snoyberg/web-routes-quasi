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

waiSite :: (((String -> Application) -> Application) -> Application -> args -> Site url Application)
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

grabMethod :: WaiArgs -> (String -> Application) -> Application
grabMethod wa f req = f (readMethod $ requestMethod req) req

readMethod :: Method -> String
readMethod = unpack . methodToBS

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
