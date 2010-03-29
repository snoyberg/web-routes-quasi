{-# LANGUAGE QuasiQuotes #-}
import Web.Routes.Quasi

main :: IO ()
main = print [$parseRoutes|
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
