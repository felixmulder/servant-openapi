module Main where

import OpenAPI.Internal.Types
import Data.Yaml as Yaml

main :: IO ()
main = do
  _petstore1 <- decodeFileThrow @_ @OpenAPI "examples/petstore.yaml"
  _petstore2 <- decodeFileThrow @_ @OpenAPI "examples/petstore-expanded.yaml"
  pure ()
