module Web.Routes.Quasi
    (
      -- * Quasi quoter
      parseRoutes
    , parseRoutesNoCheck
      -- * Template haskell
      -- ** Low level
    , createRoutes
    , createRender
    , createParse
      -- * Type classes
    , SinglePiece (..)
    , MultiPiece (..)
    , Strings
    ) where

import Web.Routes.Quasi.TH
import Web.Routes.Quasi.Parse
import Web.Routes.Quasi.Classes
