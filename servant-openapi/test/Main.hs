module Main where

import OpenAPI.Internal.Types
import Servant.OpenAPI.Internal
import OpenAPI.Internal.Class
import Data.Yaml as Yaml
import Data.Text (Text)
import Data.Proxy
import GHC.Generics
import qualified Data.ByteString.Char8 as BS

import Servant.API

main :: IO ()
main = do
  let endpoints = toEndpoints $ Proxy @API
  print endpoints
  BS.putStrLn ("\n" <> Yaml.encode endpoints)



type API = CatAPI :<|> DogAPI :<|> DeleteDogAPI

type CatAPI
  = "cats"
  :> Header "collar" Text
  :> QueryParam "ignored_instruction" Text
  :> QueryFlag "come_here"
  :> ReqBody '[JSON] Cat
  :> PutCreated '[JSON] Cat

type DogAPI
  = "dogs"
  :> Header' '[Required] "leash" Text
  :> Capture "dog_name" Text
  :> QueryFlag "good_boy"
  :> ReqBody '[JSON] Dog
  :> PutCreated '[JSON] (Headers '[Header "Location" Text] Dog)

type DeleteDogAPI
  = "dogs"
  :> Capture "dog_name" Text
  :> Delete '[JSON] NoContent

data Dog = Dog
  { name :: Text
  , age :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToOpenAPISchema)

data Color = Black | White | Brown
  deriving stock (Show, Generic)
  deriving anyclass (ToOpenAPISchema)


data Cat = Cat
  { name :: Text
  , color :: Color
  , bro :: Maybe Dog
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToOpenAPISchema)

data User = Anonymous Text | LoggedInUser Int Text
  deriving stock (Show, Generic)
  deriving anyclass (ToOpenAPISchema)
