{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
import Web.Routes.Quasi
import Test.QuickCheck
import Control.Applicative
import Network.Wai (Application, Response (..), Status (..))
import Network.Wai.Enumerator
import Network.Wai.Handler.CGI
import Data.ByteString.Lazy.Char8 (pack)

$(createRoutes "MyRoutes" ''Application ''Int [$parseRoutes|
/                    Home       GET
/user/#userid        User       GET PUT DELETE
/static              Static     StaticRoutes staticRoutes
/foo/*slurp          Foo
/bar/$barparam       Bar
|])

handleFoo :: Int -> (MyRoutes -> String) -> [String] -> Application
handleFoo = undefined

handleBar :: Int -> (MyRoutes -> String) -> String -> Application
handleBar = undefined

getHome :: Int -> (MyRoutes -> String) -> Application
getHome i s _ = return $ Response
                    Status200
                    []
                    $ Right $ fromLBS $ pack $ show
                        [ show i
                        , s $ Foo ["bar baz+bin"]
                        ]

getUser :: Int -> (MyRoutes -> String) -> Integer -> Application
getUser = undefined

putUser :: Int -> (MyRoutes -> String) -> Integer -> Application
putUser = undefined

deleteUser :: Int -> (MyRoutes -> String) -> Integer -> Application
deleteUser = undefined

badMethod :: Application
badMethod = undefined

main :: IO ()
main = do
    quickCheck $ \s -> parseMyRoutes (renderMyRoutes s) == Right s

    run $ dispatchMyRoutes badMethod 20 GET (show . renderMyRoutes) Home

    print $ User 5
    print $ parseMyRoutes ["user", "6"]
    print $ parseMyRoutes ["invalid", "route"]
    print $ parseMyRoutes ["foo", "six", "seven", "8"]
    print $ renderMyRoutes Home
    print $ renderMyRoutes $ User 6
    print $ renderMyRoutes $ Foo ["bar baz", "bin"]
    print $ parseMyRoutes ["user", "six"]

instance Arbitrary MyRoutes where
    arbitrary = oneof
        [ return Home
        , User <$> arbitrary
        , return Static -- <$> arbitrary
        , Foo <$> arbitrary
        , Bar <$> arbitrary
        ]
