{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Main
    ( main
    , waiSite
    ) where

import Test.Framework (defaultMain)
import Test.Framework.Providers.QuickCheck2

import Web.Routes.Quasi
import Test.QuickCheck
import Control.Applicative
import Network.Wai
import Network.Wai.Enumerator
--import Network.Wai.Handler.SimpleServer
import Data.ByteString.Lazy.Char8 (pack)
import Data.ByteString.Char8 (unpack)
import Web.Routes.Site
import Language.Haskell.TH.Syntax
import Web.Encodings

data StaticArgs = StaticArgs

getStaticArgs :: Int -> StaticArgs
getStaticArgs _ = StaticArgs

data StaticRoutes = StaticRoutes { unStaticRoutes :: [String] }
    deriving (Show, Read, Eq)

type instance Routes StaticArgs = StaticRoutes

siteStatic :: QuasiSite Application StaticArgs marg
siteStatic = QuasiSite
    undefined
    unStaticRoutes
    (Right . StaticRoutes)

type Explode surl murl marg = (murl -> String)
                           -> surl
                           -> (surl -> murl)
                           -> marg
                           -> (marg -> Int)
                           -> Application
                           -> String
                           -> Application
explode :: Explode surl murl marg -> Explode surl murl marg
explode = id

fmap (\(QuasiSiteDecs a b c d) -> [a, b, c, d])
  $ createQuasiSite QuasiSiteSettings
    { crRoutes = mkName "MyRoutes"
    , crApplication = ConT ''Application
    , crArgument = ConT ''Int
    , crExplode = VarE $ mkName "explode"
    , crResources = [$parseRoutes|
/                    Home       GET
/user/#Integer       User       GET PUT DELETE
/static              Static     StaticArgs siteStatic getStaticArgs
/foo/*Strings        Foo
/bar/#String         Bar
|]
    , crSite = mkName "theSite"
    , crMaster = Left $ ConT ''Int
    }

handleFoo :: [String] -> Explode MyRoutes murl marg
handleFoo a mrender _surl tomurl _marg _tosarg _badmethod _method _req = return $ Response
                    Status200
                    []
                    $ Right $ fromLBS $ pack $ show ("in foo", mrender $ tomurl $ User 78, a)

handleBar :: String -> Explode MyRoutes murl marg
handleBar a mrender _surl tomurl _marg _tosarg _badmethod _method _req = return $ Response
                    Status200
                    []
                    $ Right $ fromLBS $ pack $ show ("in bar", mrender $ tomurl $ User 78, a)

getHome :: Explode MyRoutes murl marg
getHome mrender _surl tomurl marg tosarg _badmethod _method _req = return $ Response
                    Status200
                    []
                    $ Right $ fromLBS $ pack $ show
                        [ show $ tosarg marg
                        , mrender $ tomurl $ Foo ["bar baz+bin"]
                        ]

getUser :: Integer -> Explode MyRoutes murl marg
getUser uid mrender _surl tomurl marg tosarg _badmethod _method _req = return $ Response
                    Status200
                    []
                    $ Right $ fromLBS $ pack $ unlines
                        [ "Getting user"
                        , show uid
                        , show $ tosarg marg
                        , mrender $ tomurl Home
                        ]

putUser :: Integer -> Explode MyRoutes murl marg
putUser = undefined

deleteUser :: Integer -> Explode MyRoutes murl marg
deleteUser = undefined

msg404 :: String -> Application
msg404 s _ = return $ Response
                    Status404
                    []
                    $ Right $ fromLBS $ pack $ "Not found: " ++ s

{-
badMethod :: Application
badMethod _ = return $ Response
                    Status200
                    []
                    $ Right $ fromLBS $ pack "Bad method"
-}

main :: IO ()
main = defaultMain
    [ testSuite
    , testProperty "parse/render id" $
        \r -> quasiParse theSite (quasiRender theSite r) == Right r
    ]

instance Arbitrary MyRoutes where
    arbitrary = oneof
        [ return Home
        , User <$> arbitrary
        , Static . StaticRoutes <$> arbitrary
        , Foo <$> arbitrary
        , Bar <$> arbitrary
        ]

waiSite :: Site a Application -> Application
waiSite s req = do
    let pi' = unpack $ pathInfo req
        pis = splitPath $ drop1Slash pi'
        url = parsePathSegments s pis
    case url of
        Left s' -> msg404 s' req
        Right u -> handleSite s (joinPath "http://localhost:3000/" . formatPathSegments s) u req

joinPath :: String -> [String] -> String
joinPath s [] = s
joinPath s (x:xs) = joinPath (s ++ encodeUrl x ++ "/") xs

splitPath :: String -> [String]
splitPath "" = []
splitPath x =
    let (y, z) = break (== '/') x
     in y : splitPath (drop1Slash z)

drop1Slash :: String -> String
drop1Slash ('/':x) = x
drop1Slash x = x
