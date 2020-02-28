module Main where

import Servant.OpenAPI.Internal.Types
import Data.Yaml

main :: IO ()
main = do
  petstore <- decodeFileThrow "examples/petstore.yaml"
  print @OpenAPI petstore
