{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards  #-}

module OpenAPI.Internal.References where

import           Control.Lens
import           Control.Monad.Trans.Writer
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe                 ()
import           Data.Text                  (Text)
import           OpenAPI.Internal.Types

type SchemaEnv = [(Text, SchemaObject)]

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
      = fmap (Map.fromList . over (mapped . _2) Concrete)
      . runWriter
      $ gatherAndPrune api

-- | Traverse over all schemas in the 'OpenAPI' applying 'prune'.
gatherAndPrune :: OpenAPI -> Writer SchemaEnv OpenAPI
gatherAndPrune = allPathItems . allOperations . operationSchemas $ prune

-- | Accululate the environment of all the named schemas in the OpenAPI.
gatherDefinitions :: OpenAPI -> SchemaEnv
gatherDefinitions =
  foldMapOf
    (allPathItems . allOperations . operationSchemas . #_Concrete)
    fromSchema


-- | Replace a named schema with a reference, and add it to the accumulated environment.
--   Defined as a Kleisli arrow so that it can be lifted with 'Traversal''.
prune :: ReferenceOr SchemaObject -> Writer SchemaEnv (ReferenceOr SchemaObject)
prune = \case
  Ref r -> pure (Ref r)
  Concrete s -> case view #title s of
    Nothing -> pure (Concrete s)
    Just name -> do
      tell [(name, s)]
      pure . Ref . ReferenceObject $ localRefPrefix <> name
  where
    localRefPrefix :: Text
    localRefPrefix = "#/components/schemas/"

fromSchema :: SchemaObject -> SchemaEnv
fromSchema schema = case view #title schema of
  Just name -> [(name, schema)]
  Nothing -> []

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
