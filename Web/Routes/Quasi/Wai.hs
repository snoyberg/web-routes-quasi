{-# LANGUAGE TemplateHaskell #-}
module Web.Routes.Quasi.Wai where

import Network.Wai
import Network.Wai.Enumerator
import Web.Routes
import Web.Routes.Quasi

import Data.ByteString.Char8 (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as L

drop1Slash :: String -> String
drop1Slash ('/':x) = x
drop1Slash x = x

data WaiArgs = WaiArgs
    { waiApproot :: String -- ^ include trailing slash
    , waiBadMethod :: Application
    , wai404 :: Application
    }

waiSite :: QuasiSite Application args args
        -> WaiArgs
        -> args
        -> Application
waiSite site wa args req = do
    let method = readMethod $ requestMethod req
    let pieces = filter (not . null)
               $ decodePathInfo $ drop1Slash $ unpack $ pathInfo req
    print ("pieces", pieces)
    case quasiParse site pieces of
        Left _ -> wai404 wa req
        Right url ->
            let format u = waiApproot wa ++ encodePathInfo
                            (quasiRender site u)
             in quasiDispatch site format url id args id (waiBadMethod wa) method req

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
