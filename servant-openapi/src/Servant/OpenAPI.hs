module Servant.OpenAPI
  ( -- * Interface for generating OpenAPI definition
    HasOpenAPI
  , toOpenAPI
  , toBareOpenAPI

  , blankInfo
  , blankOpenAPI

    -- * API Types
  ) where

import Servant.OpenAPI.Internal
