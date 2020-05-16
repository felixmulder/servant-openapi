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
  ) where

import OpenAPI.Internal.Class
import OpenAPI.Internal.Types
import OpenAPI.Internal.References
