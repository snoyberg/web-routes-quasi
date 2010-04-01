{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
import Web.Routes.Quasi
import Test.QuickCheck
import Control.Applicative

$(createRoutes "MyRoutes" [$parseRoutes|
/                    Home       GET
/user/#userid        User       GET PUT DELETE
/static              Static     StaticRoutes staticRoutes
/foo/*slurp          Foo
/bar/$barparam       Bar
|])

main :: IO ()
main = do
    quickCheck $ \s -> parseMyRoutes (renderMyRoutes s) == Right s

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
