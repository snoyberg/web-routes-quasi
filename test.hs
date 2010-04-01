{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
import Web.Routes.Quasi

resources :: [Resource]
resources = [$parseRoutes|
/                    Home       GET
/user/#userid        User       GET PUT DELETE
/static              Static     StaticRoutes staticRoutes
/foo/*slurp          Foo
/bar/$barparam       Bar
|]

$(createRoutes "MyRoutes" [$parseRoutes|
/                    Home       GET
/user/#userid        User       GET PUT DELETE
/static              Static     StaticRoutes staticRoutes
/foo/*slurp          Foo
/bar/$barparam       Bar
|])

main :: IO ()
main = do
    print resources
    print $ User 5
    print $ parseMyRoutes ["user", "6"]
    print $ parseMyRoutes ["invalid", "route"]
    print $ parseMyRoutes ["foo", "six", "seven", "8"]
    print $ parseMyRoutes ["user", "six"]
