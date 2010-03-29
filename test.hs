{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
import Web.Routes.Quasi

resources = [$parseRoutes|
/:
  name: Home
  methods: [GET]
/user/#userid:
  name: User
  methods: [GET, PUT, DELETE]
/static:
  name: Static
  subsite: StaticRoutes
  dispatch: staticRoutes
/foo/*slurp:
  name: Foo
/bar/$barparam:
  name: Bar
|]

$(createRoutes "MyRoutes" [$parseRoutes|
/:
  name: Home
  methods: [GET]
/user/#userid:
  name: User
  methods: [GET, PUT, DELETE]
/static:
  name: Static
  subsite: StaticRoutes
  dispatch: staticRoutes
/foo/*slurp:
  name: Foo
/bar/$barparam:
  name: Bar
|])

main :: IO ()
main = do
    print resources
    print $ User 5
