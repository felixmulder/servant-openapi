{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell  #-}

module Main where

import           Control.Lens
import qualified Data.Map.Strict             as Map
import           Data.Proxy
import           Data.Text                   (Text)
import           GHC.Generics                (Generic)
import           Hedgehog
import           Hedgehog.Main               (defaultMain)
import           OpenAPI.Internal.Class
import           OpenAPI.Internal.References
import           Servant.API
import           Servant.OpenAPI.Internal

main :: IO ()
main = do
    defaultMain [checkParallel $$(discover)]

prop_request_body_distributes :: Property
prop_request_body_distributes = once . property $ do
  toEndpointInfo (Proxy @Dog1) === toEndpointInfo (Proxy @Dog2)
  let dog1 = toEndpointInfo (Proxy @Dog1)
      dog2 = toEndpointInfo (Proxy @Dog2)
      Just pathItem1 = dog1 Map.!? "/dog"
      Just pathItem2 = dog2 Map.!? "/dog"
  pathItem1 === pathItem2

type Dog1 =
  "dog" :> ReqBody '[JSON] Dog :>
    (Put '[JSON] Dog :<|> Post '[JSON] Dog)

type Dog2 =
       "dog" :> ReqBody '[JSON] Dog :> Put '[JSON] Dog
  :<|> "dog" :> ReqBody '[JSON] Dog :> Post '[JSON] Dog


prop_static_path_segments_distribute :: Property
prop_static_path_segments_distribute = once . property $ do
  toEndpointInfo (Proxy @Cat1) === toEndpointInfo (Proxy @Cat2)
  let cat1 = toEndpointInfo (Proxy @Cat1)
      cat2 = toEndpointInfo (Proxy @Cat2)
  cat1 === cat2
  Map.keys cat1 === ["/cat/{name}", "/cat/adopted"]

prop_pruneAndReference_example :: Property
prop_pruneAndReference_example = once . property $ do
  let openApi = pruneAndReference . toBareOpenAPI $ Proxy @Cat1
  openApi ^.. #components . _Just . #schemas . _Just . traverse . #_Concrete ===
      [ pruneSchema . toSchema $ Proxy @Cat
      , pruneSchema . toSchema $ Proxy @Dog
      ]


type Cat1 =
  "cat" :>
    (    Capture "name" Text :> ReqBody '[JSON] Cat :> Put '[JSON] Cat
    :<|> "adopted" :> Get '[JSON] [Cat]
    )

type Cat2 =
       "cat" :> Capture "name" Text :> ReqBody '[JSON] Cat :> Put '[JSON] Cat
  :<|> "cat" :> "adopted" :> Get '[JSON] [Cat]



data Dog = Dog
  { name :: Text
  , age :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToOpenAPISchema)

data Cat = Cat
  { name :: Text
  , age :: Int
  , bro :: Maybe Dog
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToOpenAPISchema)


once :: Property -> Property
once = withTests 1
