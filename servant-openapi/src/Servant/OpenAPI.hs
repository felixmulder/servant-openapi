module Servant.OpenAPI
  ( -- * Interface for generating OpenAPI definition
    HasOpenAPI(..)

  , HasOpenAPIEndpointInfo(..)
  , toBareOpenAPI

  , ToOpenAPIMetadata
  , AddOpenAPIMetadata

  , blankInfo
  , blankOpenAPI

    -- * API Types
  ) where

import Servant.OpenAPI.Internal
