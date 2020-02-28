module Servant.OpenAPI.Internal where

import           Data.Generics.Labels           ()
import           Data.Proxy
import           Servant.OpenAPI.Internal.Types


class HasAPISchema api where
  toAPISchema :: Proxy api -> OpenAPI
