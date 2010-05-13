{-# LANGUAGE OverloadedStrings #-}
import Blog
import Static
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
    putStrLn "Running..."
    run 3000 $ waiSite siteBlog (WaiArgs
        { waiApproot = "http://localhost:3000/"
        , waiBadMethod = defBadMethod
        , wai404 = def404
        }) (BlogArgs
        { staticPath = Static "static"
        , blogTitle = "My Blog"
        , blogEntries = entries
        })
