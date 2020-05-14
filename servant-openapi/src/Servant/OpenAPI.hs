module Servant.OpenAPI
  ( -- * Interface for materializing API definition
    HasOpenAPI(..)

    -- * API Types
  , OpenAPI
  ) where

import Data.Proxy (Proxy)
import OpenAPI.Internal.Types (OpenAPI)

class HasOpenAPI api where
  apiDefinition :: Proxy api -> OpenAPI
