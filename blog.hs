import Blog
import Network.Wai.Handler.SimpleServer
import Web.Routes.Quasi.Wai

entries :: [Entry]
entries =
    [ Entry "quasi" "Quasi" "Content for Quasi quotes"
    , Entry "yesod" "Yesod" "Yesod released"
    , Entry "wai" "Web Application Interface" "Some content on this"
    ]

main :: IO ()
main = do
    run 3000 $ waiSite siteBlogRoutes (WaiArgs
        { waiApproot = "http://localhost:3000/"
        , waiBadMethod = defBadMethod
        , wai404 = def404
        }) (BlogArgs
        { staticPath = "static"
        , blogTitle = "My Blog"
        , blogEntries = entries
        })
