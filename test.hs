import Web.Routes.Quasi

import Data.Attempt
import Data.Object.Yaml

main :: IO ()
main = do
    x <- decodeFile "test.yaml"
    fa (resourcesFromSO x) >>= print
