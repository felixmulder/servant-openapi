{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards  #-}

module OpenAPI.Internal.References where

import           Prelude hiding (not, maximum, minimum)

import           Control.Lens hiding (enum, allOf, anyOf)
import           Control.Monad.Trans.Accum
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Maybe                 ()
import           Data.Text                  (Text)
import           OpenAPI.Internal.Types
import           GHC.Generics
import           Control.Monad (unless)

nestedSchemas :: Traversal' SchemaObject (ReferenceOr SchemaObject)
nestedSchemas f SchemaObject{..} =
  SchemaObject
    <$> pure title
    <*> pure type_
    <*> pure discriminator
    <*> pure multipleOf
    <*> pure maximum
    <*> pure exclusiveMaximum
    <*> pure minimum
    <*> pure exclusiveMinimum
    <*> pure maxLength
    <*> pure minLength
    <*> pure pattern
    <*> pure maxItems
    <*> pure minItems
    <*> pure uniqueItems
    <*> pure maxProperties
    <*> pure minProperties
    <*> pure required
    <*> pure enum
    <*> (_Just . traverse @[]) f allOf
    <*> (_Just . traverse @[]) f oneOf
    <*> (_Just . traverse @[]) f anyOf
    <*> (_Just . traverse @[]) f not
    <*> _Just f items
    <*> (_Just . #unProperties . traverse @(Map Text)) f properties
    <*> (_Just . _Right) f additionalProperties
    <*> pure description
    <*> pure format
    <*> pure default_


-- | Add all named schemas to the '#components' environment, and replace
--   their occurrences with references.
pruneAndReference :: OpenAPI -> OpenAPI
pruneAndReference api =
  prunedApi
    { components = Just
        emptyComponents
          { schemas = Just env }
    }
  where
    env :: Map Text (ReferenceOr SchemaObject)
    prunedApi :: OpenAPI
    (prunedApi, env)
      = fmap (fmap Concrete . definitions)
      . flip runAccum mempty
      $ defineAndPruneAll api

-- | Traverse over all schemas in the 'OpenAPI' applying 'prune'.
defineAndPruneAll :: OpenAPI -> Accum DefinitionEnv OpenAPI
defineAndPruneAll = allPathItems . allOperations . operationSchemas $ defineAndPrune


localRefPrefix :: Text
localRefPrefix = "#/components/schemas/"

-- | Add the present schema, along with all named schemas that occur
--   nested within it, in referenced form.
--   Defined as a Kleisli arrow so that it can be lifted with 'Traversal''.
defineAndPrune :: ReferenceOr SchemaObject -> Accum DefinitionEnv (ReferenceOr SchemaObject)
defineAndPrune = \case
  Ref r -> pure (Ref r)
  Concrete s -> case view #title s of
    Nothing -> pure (Concrete s)
    Just name -> do
      env <- look
      -- Always return the reference, regardless of other actions from this point
      toReference name <$ case lookupDefinition name env of
        Just _envSchema -> pure ()
        Nothing -> unless (name `Set.member` ongoing env) $ do
          -- Though we are not done with the present schema, we guard against recursing
          -- into the same definition again by adding it to the 'ongoing' list first
          add $ newOngoing name
          referencedSchema <- nestedSchemas defineAndPrune s
          add $ newDefinition name referencedSchema

pruneSchema :: SchemaObject -> SchemaObject
pruneSchema = over nestedSchemas $ \case
  Ref n -> Ref n
  Concrete s -> case view #title s of
    Nothing -> Concrete s
    Just name -> toReference name

toReference :: Text -> ReferenceOr a
toReference name = Ref . ReferenceObject $ localRefPrefix <> name

lookupDefinition :: Text -> DefinitionEnv -> Maybe SchemaObject
lookupDefinition name DefinitionEnv{definitions} =
  definitions Map.!? (localRefPrefix <> name)

newDefinition :: Text -> SchemaObject -> DefinitionEnv
newDefinition name schema = DefinitionEnv (Map.singleton name schema) mempty

newOngoing :: Text -> DefinitionEnv
newOngoing name = DefinitionEnv mempty $ Set.singleton name

data DefinitionEnv = DefinitionEnv
  { definitions :: Map Text SchemaObject
  , ongoing :: Set Text
  } deriving stock (Show, Eq, Generic)

instance Semigroup DefinitionEnv where
    d1 <> d2 = DefinitionEnv
      { definitions = definitions d1 <> definitions d2
      , ongoing = ongoing d1 <> ongoing d2
      }
instance Monoid DefinitionEnv where
    mempty = DefinitionEnv mempty mempty


-- | Traversal over nested schemas from 'OperationObject':
--     * parameters
--     * requestBody
--     * responses
operationSchemas :: Traversal' OperationObject (ReferenceOr SchemaObject)
operationSchemas f OperationObject{..} =
  OperationObject
    <$> pure tags
    <*> pure summary
    <*> pure description
    <*> pure externalDocs
    <*> pure operationId
    <*> parameterSchemas f parameters
    <*> requestBodySchemas f requestBody
    <*> responsesSchemas f responses
    <*> pure callbacks
    <*> pure deprecated
    <*> pure security
    <*> pure servers

  where
    parameterSchemas
      = _Just
      . traverse
      . #_Concrete
      . #schema
      . _Just

    responsesSchemas
      = #unResponsesObject
      . traverse
      . #_Concrete
      . responseSchemas

    requestBodySchemas
      = _Just
      . #_Concrete
      . #content
      . traverse
      . #schema
      . _Just


allPathItems :: Traversal' OpenAPI PathItemObject
allPathItems = #paths . traverse

-- | Traversal over nested schemas from the 'ResponseObject':
--     * 'headers'
--     * 'content'
responseSchemas :: Traversal' ResponseObject (ReferenceOr SchemaObject)
responseSchemas f ResponseObject{..} =
  ResponseObject
    <$> pure description
    <*> headerSchemas f headers
    <*> contentSchemas f content
    <*> pure links

  where
    headerSchemas
      = _Just
      . traverse
      . #_Concrete
      . #schema
      . _Just

    contentSchemas
      = _Just
      . traverse
      . #schema
      . _Just
