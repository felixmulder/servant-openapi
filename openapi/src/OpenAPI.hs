module OpenAPI
  ( -- * Type classes for schemas
  ToOpenAPISchema(..)
  -- ** Generic schema class
  , GToOpenAPI(..)
  -- *** Generic class configuration
  , GenericSchemaOptions(..)
  , SumEncoding(..)
  , defaultSchemaOptions
  -- *** Entrypoint function for schema deriving
  , genericToSchema

  -- * Handling References
  , pruneAndReference
  , gatherDefinitions
  , SchemaEnv

  -- * Types
  , module OpenAPI.Internal.Types

  -- * Conveniences
  , renderSchemaYaml
  , renderSchemaJson
  , writeSchemaYaml
  , writeSchemaJson
  ) where

import OpenAPI.Internal.Class
import OpenAPI.Internal.Types
import OpenAPI.Internal.References

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Yaml as Yaml
import qualified Data.Aeson as Aeson

renderSchemaYaml :: OpenAPI -> BS.ByteString
renderSchemaYaml = Yaml.encode

renderSchemaJson :: OpenAPI -> BSL.ByteString
renderSchemaJson = Aeson.encode

writeSchemaYaml :: FilePath -> OpenAPI -> IO ()
writeSchemaYaml path = BS.writeFile path . renderSchemaYaml

writeSchemaJson :: FilePath -> OpenAPI -> IO ()
writeSchemaJson path = BSL.writeFile path . renderSchemaJson
