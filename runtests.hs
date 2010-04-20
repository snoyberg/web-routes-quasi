{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
import Web.Routes.Quasi
import Test.QuickCheck
import Control.Applicative
import Network.Wai
import Network.Wai.Enumerator
import Network.Wai.Handler.SimpleServer
import Data.ByteString.Lazy.Char8 (pack)
import Data.ByteString.Char8 (unpack)
import Web.Routes.Site
import Web.Encodings
import Language.Haskell.TH.Syntax

data StaticArgs = StaticArgs

getStaticArgs _ = StaticArgs

data StaticRoutes = StaticRoutes { unStaticRoutes :: [String] }
    deriving (Show, Read, Eq)

siteStatic :: Site StaticRoutes
           ( String
          -> Application
          -> StaticArgs
          -> (StaticRoutes -> mroutes)
          -> (mroutes -> String)
          -> master
          -> Application
           )
siteStatic = Site
    undefined
    unStaticRoutes
    (Right . StaticRoutes)

createRoutes CreateRoutesSettings
    { crRoutes = mkName "MyRoutes"
    , crApplication = ConT ''Application
    , crArgument = ConT ''Int
    , crExplode = VarE $ mkName "id"
    , crResources = [$parseRoutes|
/                    Home       GET
/user/#userid        User       GET PUT DELETE
/static              Static     StaticRoutes siteStatic getStaticArgs
/foo/*slurp          Foo
/bar/$barparam       Bar
|]
    , crSite = mkName "theSite"
    }

handleFoo :: [String] -> Int -> MyRoutes -> (MyRoutes -> String) -> Application
handleFoo a _ _ s _ = return $ Response
                    Status200
                    []
                    $ Right $ fromLBS $ pack $ show ("in foo", s $ User 78, a)

handleBar :: String -> Int -> MyRoutes -> (MyRoutes -> String) -> Application
handleBar a _ _ s _ = return $ Response
                    Status200
                    []
                    $ Right $ fromLBS $ pack $ show ("in bar", s $ User 78, a)

getHome :: Int -> MyRoutes -> (MyRoutes -> String) -> Application
getHome i _ s _ = return $ Response
                    Status200
                    []
                    $ Right $ fromLBS $ pack $ show
                        [ show i
                        , s $ Foo ["bar baz+bin"]
                        ]

getUser :: Integer -> Int -> MyRoutes -> (MyRoutes -> String) -> Application
getUser uid i _ s _ = return $ Response
                    Status200
                    []
                    $ Right $ fromLBS $ pack $ unlines
                        [ "Getting user"
                        , show uid
                        , show i
                        , s Home
                        ]

putUser :: Integer -> Int -> MyRoutes -> (MyRoutes -> String) -> Application
putUser = undefined

deleteUser :: Integer -> Int -> MyRoutes -> (MyRoutes -> String) -> Application
deleteUser = undefined

msg404 :: String -> Application
msg404 s _ = return $ Response
                    Status404
                    []
                    $ Right $ fromLBS $ pack $ "Not found: " ++ s

badMethod :: Application
badMethod _ = return $ Response
                    Status200
                    []
                    $ Right $ fromLBS $ pack "Bad method"

{-
site :: Site MyRoutes Application
site = siteMyRoutes getMethod badMethod 20
-}

getMethod :: (String -> Application) -> Application
getMethod m = m "GET" -- FIXME

main :: IO ()
main = do
    let parseMyRoutes = parsePathSegments theSite
    let renderMyRoutes = formatPathSegments theSite

    quickCheck $ \s -> parseMyRoutes (renderMyRoutes s) == Right s

    --run $ dispatchMyRoutes badMethod 20 GET (show . renderMyRoutes 20) Home

    print $ User 5
    print $ parseMyRoutes ["user", "6"]
    print $ parseMyRoutes ["invalid", "route"]
    print $ parseMyRoutes ["foo", "six", "seven", "8"]
    print $ parseMyRoutes ["static", "foo", "six", "seven", "8"]
    print $ renderMyRoutes Home
    print $ renderMyRoutes $ User 6
    print $ renderMyRoutes $ Foo ["bar baz", "bin"]
    print $ parseMyRoutes ["user", "six"]

    --run 3000 $ waiSite site --"http://localhost:3000"

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
