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

data StaticRoutes = StaticRoutes { unStaticRoutes :: [String] }
    deriving (Show, Read, Eq)

staticRoutes :: Int -> Site StaticRoutes Application
staticRoutes _ = Site
    undefined
    Nothing
    unStaticRoutes
    (Right . StaticRoutes)

$(createRoutes "MyRoutes" ''Application ''Int [$parseRoutes|
/                    Home       GET
/user/#userid        User       GET PUT DELETE
/static              Static     StaticRoutes staticRoutes
/foo/*slurp          Foo
/bar/$barparam       Bar
|])

handleFoo :: Int -> (MyRoutes -> String) -> [String] -> Application
handleFoo _ s a _ = return $ Response
                    Status200
                    []
                    $ Right $ fromLBS $ pack $ show ("in foo", s $ User 78, a)

handleBar :: Int -> (MyRoutes -> String) -> String -> Application
handleBar _ s a _ = return $ Response
                    Status200
                    []
                    $ Right $ fromLBS $ pack $ show ("in bar", s $ User 78, a)

getHome :: Int -> (MyRoutes -> String) -> Application
getHome i s _ = return $ Response
                    Status200
                    []
                    $ Right $ fromLBS $ pack $ show
                        [ show i
                        , s $ Foo ["bar baz+bin"]
                        ]

getUser :: Int -> (MyRoutes -> String) -> Integer -> Application
getUser i s uid _ = return $ Response
                    Status200
                    []
                    $ Right $ fromLBS $ pack $ unlines
                        [ "Getting user"
                        , show uid
                        , show i
                        , s Home
                        ]

putUser :: Int -> (MyRoutes -> String) -> Integer -> Application
putUser = undefined

deleteUser :: Int -> (MyRoutes -> String) -> Integer -> Application
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

site :: Site MyRoutes Application
site = siteMyRoutes getMethod badMethod 20

getMethod :: (String -> Application) -> Application
getMethod m = m "GET" -- FIXME

main :: IO ()
main = do
    quickCheck $ \s -> parseMyRoutes 20 (renderMyRoutes 20 s) == Right s

    --run $ dispatchMyRoutes badMethod 20 GET (show . renderMyRoutes 20) Home

    print $ User 5
    print $ parseMyRoutes 20 ["user", "6"]
    print $ parseMyRoutes 20 ["invalid", "route"]
    print $ parseMyRoutes 20 ["foo", "six", "seven", "8"]
    print $ parseMyRoutes 20 ["static", "foo", "six", "seven", "8"]
    print $ renderMyRoutes 20 Home
    print $ renderMyRoutes 20 $ User 6
    print $ renderMyRoutes 20 $ Foo ["bar baz", "bin"]
    print $ parseMyRoutes 20 ["user", "six"]

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
