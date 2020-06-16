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
import           Servant.API
import           Servant.OpenAPI.Internal
import           OpenAPI

main :: IO ()
main = do
    defaultMain [checkParallel $$(discover)]

prop_request_body_distributes :: Property
prop_request_body_distributes = once . property $ do
  toBareOpenAPI (Proxy @Dog1) === toBareOpenAPI (Proxy @Dog2)
  let dog1 = toBareOpenAPI (Proxy @Dog1)
      dog2 = toBareOpenAPI (Proxy @Dog2)
      Just pathItem1 = view #paths dog1 Map.!? "/dog"
      Just pathItem2 = view #paths dog2 Map.!? "/dog"
  pathItem1 === pathItem2

type Dog1 =
  "dog" :> ReqBody '[JSON] Dog :>
    (Put '[JSON] Dog :<|> Post '[JSON] Dog)

type Dog2 =
       "dog" :> ReqBody '[JSON] Dog :> Put '[JSON] Dog
  :<|> "dog" :> ReqBody '[JSON] Dog :> Post '[JSON] Dog


prop_static_path_segments_distribute :: Property
prop_static_path_segments_distribute = once . property $ do
  let cat1 = toBareOpenAPI (Proxy @Cat1)
      cat2 = toBareOpenAPI (Proxy @Cat2)
  cat1 === cat2
  Map.keys (view #paths cat1) === ["/cat/{name}", "/cat/adopted"]
  toBareOpenAPI (Proxy @Cat1) === toBareOpenAPI (Proxy @Cat2)

prop_pruneAndReference_example :: Property
prop_pruneAndReference_example = once . property $ do
  let openApi = toBareOpenAPI $ Proxy @Cat1
  annotateShow openApi
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
  , sis :: Maybe Cat
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
