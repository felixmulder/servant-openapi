{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
module Servant.OpenAPI.Internal.Types
  ( -- * Open API
    OpenAPI(..)
    -- ** OpenAPI lenses
  , apiInfo
  , apiServers
  , apiPaths
  , apiComponents
  , apiSecurity
  , apiTags
  , apiExternalDocs

    -- * Info Object
  , InfoObject(..)
    -- ** Info object lenses
  , infoTitle
  , infoDescription
  , infoTermsOfService
  , infoContact
  , infoLicense
  , infoVersion

    -- * Server Object
  , ServerObject(..)
    -- ** Server object lenses
  , serverUrl
  , serverDescription

    -- * Tag object
  , TagObject(..)
    -- ** Tag object lenses
  , tagName
  , tagDescription
  , tagExternalDocs

    -- * External documentation object
  , ExternalDocumentationObject(..)
    -- ** External documentation object lenses
  , externalDocsUrl
  , externalDocsDescription

    -- * License object
  , LicenseObject(..)
    -- ** License object lenses
  , licenseName
  , licenseUrl

    -- * Contact object
  , ContactObject(..)
    -- ** Contact object lenses
  , contactName
  , contactUrl
  , contactEmail
  ) where

import           Control.Lens.Type (Lens')
import qualified Data.Aeson as Aeson (Value)
import           Data.Generics.Labels ()
import           Data.Map.Strict (Map)
import           Data.Text (Text)
import           GHC.Generics (Generic)

data OpenAPI = OpenAPI
  { openapi :: Text
    -- ^ This string MUST be the semantic version number of the OpenAPI
    --   Specification version that the OpenAPI document uses. The openapi field
    --   SHOULD be used by tooling specifications and clients to interpret the
    --   OpenAPI document. This is not related to the API info.version string.
  , info :: InfoObject
    -- ^ Provides metadata about the API. The metadata MAY be used by tooling
    --   as required
  , servers :: Maybe [ServerObject]
    -- ^ An array of Server Objects, which provide connectivity information to
    --   a target server. If the servers property is not provided, or is an empty
    --   array, the default value would be a Server Object with a url value of
    --   @/@.
  , paths :: Map PathPattern PathItemObject
    -- ^ The available paths and operations for the API
  , compontents :: Maybe ComponentsObject
    -- ^ An element to hold various schemas for the specification
  , security :: Maybe [SecurityRequirementObject]
    -- ^ A declaration of which security mechanisms can be used across the API.
    --   The list of values includes alternative security requirement objects
    --   that can be used. Only one of the security requirement objects need to
    --   be satisfied to authorize a request. Individual operations can override
    --   this definition. To make security optional, an empty security
    --   requirement ({}) can be included in the array.
  , tags :: Maybe [TagObject]
    -- ^ A list of tags used by the specification with additional metadata. The
    --   order of the tags can be used to reflect on their order by the parsing
    --   tools. Not all tags that are used by the Operation Object must be
    --   declared. The tags that are not declared MAY be organized randomly or
    --   based on the tools' logic. Each tag name in the list MUST be unique.
  , externalDocs :: Maybe ExternalDocumentationObject
    -- ^ Additional external documentation
  }
  deriving stock (Generic)

apiInfo :: Lens' OpenAPI InfoObject
apiInfo = #info

apiServers :: Lens' OpenAPI (Maybe [ServerObject])
apiServers = #servers

apiPaths :: Lens' OpenAPI (Map PathPattern PathItemObject)
apiPaths = #paths

apiComponents :: Lens' OpenAPI (Maybe ComponentsObject)
apiComponents = #compontents

apiSecurity :: Lens' OpenAPI (Maybe [SecurityRequirementObject])
apiSecurity = #security

apiTags :: Lens' OpenAPI (Maybe [TagObject])
apiTags = #tags

apiExternalDocs :: Lens' OpenAPI (Maybe ExternalDocumentationObject)
apiExternalDocs = #externalDocs

data InfoObject = InfoObject
  { title :: Text
    -- ^ The title of the API
  , description :: Maybe Text
    -- ^ A short description of the API. CommonMark syntax MAY be used for rich
    --   text representation.
  , termsOfService :: Maybe Text
    -- ^ A URL to the Terms of Service for the API. MUST be in the format of a
    --   URL
  , contact :: Maybe ContactObject
    -- ^ The contact information for the exposed API
  , license :: Maybe LicenseObject
    -- ^ The license information for the exposed API
  , version :: Text
    -- ^ The version of the OpenAPI document (which is distinct from the
    --   OpenAPI Specification version or the API implementation version)
  }
  deriving stock (Generic)

infoTitle :: Lens' InfoObject Text
infoTitle = #title

infoDescription :: Lens' InfoObject (Maybe Text)
infoDescription = #description

infoTermsOfService :: Lens' InfoObject (Maybe Text)
infoTermsOfService = #termsOfService

infoContact :: Lens' InfoObject (Maybe ContactObject)
infoContact = #contact

infoLicense :: Lens' InfoObject (Maybe LicenseObject)
infoLicense = #license

infoVersion :: Lens' InfoObject Text
infoVersion = #version

data ServerObject = ServerObject
  { url :: Text
    -- ^ A URL to the target host. This URL supports Server Variables and MAY
    --   be relative, to indicate that the host location is relative to the
    --   location where the OpenAPI document is being served. Variable
    --   substitutions will be made when a variable is named in @{@ brackets @}@.
  , description :: Maybe Text
    -- ^ An optional string describing the host designated by the URL.
    --   CommonMark syntax MAY be used for rich text representation.
  , variables :: Maybe (Map Text ServerVariableObject)
    -- ^ A map between a variable name and its value. The value is used for
    --   substitution in the server's URL template.
  }
  deriving stock (Generic)

serverUrl :: Lens' ServerObject Text
serverUrl = #url

serverDescription :: Lens' ServerObject (Maybe Text)
serverDescription = #description

data ServerVariableObject = ServerVariableObject
  { enum :: Maybe [Text]
    -- ^ An enumeration of string values to be used if the substitution options
    --   are from a limited set. The array SHOULD NOT be empty.
  , default_ :: Text
    -- ^ The default value to use for substitution, which SHALL be sent if an
    --   alternate value is not supplied. Note this behavior is different than
    --   the Schema Object's treatment of default values, because in those cases
    --   parameter values are optional. If the enum is defined, the value SHOULD
    --   exist in the enum's values.
  , description :: Maybe Text
    -- ^ An optional description for the server variable. CommonMark syntax MAY
    --   be used for rich text representation.
  }
  deriving stock (Generic)

data TagObject = TagObject
  { name :: Text
    -- ^ The name of the tag
  , description :: Maybe Text
    -- ^ A short description for the tag. CommonMark syntax MAY be used for
    --   rich text representation.
  , externalDocs :: Maybe ExternalDocumentationObject
    -- ^ Additional external documentation for this tag
  }
  deriving stock (Generic)

tagName :: Lens' TagObject Text
tagName = #name

tagDescription :: Lens' TagObject (Maybe Text)
tagDescription = #description

tagExternalDocs :: Lens' TagObject (Maybe ExternalDocumentationObject)
tagExternalDocs = #externalDocs

data ExternalDocumentationObject = ExternalDocumentationObject
  { url :: Text
    -- ^ A short description of the target documentation. CommonMark syntax MAY
    --   be used for rich text representation.
  , description :: Maybe Text
    -- ^ The URL for the target documentation. Value MUST be in the format of a
    --   URL
  }
  deriving stock (Generic)

externalDocsUrl :: Lens' ExternalDocumentationObject Text
externalDocsUrl = #url

externalDocsDescription :: Lens' ExternalDocumentationObject (Maybe Text)
externalDocsDescription = #description

data LicenseObject = LicenseObject
  { name :: Text
    -- ^ The license name used for the API
  , url :: Maybe Text
    -- ^ A URL to the license used for the API. MUST be in the format of a URL
  }
  deriving stock (Generic)

licenseName :: Lens' LicenseObject Text
licenseName = #name

licenseUrl :: Lens' LicenseObject (Maybe Text)
licenseUrl = #url

data ContactObject = ContactObject
  { name :: Maybe Text
    -- ^ The identifying name of the contact person/organization
  , url :: Maybe Text
    -- ^ The URL pointing to the contact information. MUST be in the format of
    --   a URL
  , email :: Maybe Text
    -- ^ The email address of the contact person/organization. MUST be in the
    --   format of an email address
  }
  deriving stock (Generic)

contactName :: Lens' ContactObject (Maybe Text)
contactName = #name

contactUrl :: Lens' ContactObject (Maybe Text)
contactUrl = #url

contactEmail :: Lens' ContactObject (Maybe Text)
contactEmail = #email

data SecurityRequirementObject
  deriving stock (Generic)

data PathPattern
  = PathVariable Text
    -- ^ A variable in the path
  | PathPart Text
    -- ^ A subpart of the path
  | PathNil
    -- ^ End of path
  deriving stock (Generic)

data PathItemObject = PathItemObject
  { ref :: Maybe Text
    -- ^ Allows for an external definition of this path item
    --
    --   The referenced structure MUST be in the format of a Path Item Object.
    --   In case a Path Item Object field appears both in the defined object
    --   and the referenced object, the behavior is undefined.
  , summary ::  Maybe Text
    -- ^ An optional, string summary, intended to apply to all operations in
    --   this path.
  , description :: Maybe Text
    -- ^ An optional, string description, intended to apply to all operations in
    --   this path. CommonMark syntax MAY be used for rich text representation.
  , get :: Maybe OperationObject
    -- ^ A definition of a GET operation on this path
  , put :: Maybe OperationObject
    -- ^ A definition of a PUT operation on this path
  , post :: Maybe OperationObject
    -- ^ A definition of a POST operation on this path
  , delete :: Maybe OperationObject
    -- ^ A definition of a DELETE operation on this path
  , options :: Maybe OperationObject
    -- ^ A definition of an OPTIONS operation on this path
  , head :: Maybe OperationObject
    -- ^ A definition of a HEAD operation on this path
  , patch :: Maybe OperationObject
    -- ^ A definition of a PATCH operation on this path
  , trace :: Maybe OperationObject
    -- ^ A definition of a TRACE operation on this path
  , servers :: Maybe [ServerObject]
    -- ^ An alternative server array to service all operations in this path.
  , parameters :: [Either ParameterObject ReferenceObject]
    -- ^ A list of parameters that are applicable for all the operations
    --   described under this path. These parameters can be overridden at the
    --   operation level, but cannot be removed there. The list MUST NOT
    --   include duplicated parameters. A unique parameter is defined by a
    --   combination of a name and location. The list can use the Reference
    --   Object to link to parameters that are defined at the OpenAPI Object's
    --   components/parameters.
  }
  deriving stock (Generic)

data OperationObject = OperationObject
  { tags :: Maybe [Text]
    -- ^ A list of tags for API documentation control. Tags can be used for
    --   logical grouping of operations by resources or any other qualifier.
  , summary :: Maybe Text
    -- ^ A short summary of what the operation does
  , description :: Maybe Text
    -- ^ A verbose explanation of the operation behavior. CommonMark syntax MAY
    --   be used for rich text representation.
  , externalDocs :: Maybe ExternalDocumentationObject
    -- ^ Additional external documentation for this operation
  , operationId :: Maybe Text
    -- ^ Unique string used to identify the operation. The id MUST be unique
    --   among all operations described in the API. The operationId value is
    --   case-sensitive. Tools and libraries MAY use the operationId to uniquely
    --   identify an operation, therefore, it is RECOMMENDED to follow common
    --   programming naming conventions.
  , parameters :: Maybe [Either ParameterObject ReferenceObject]
    -- ^ A list of parameters that are applicable for this operation. If a
    --   parameter is already defined at the Path Item, the new definition will
    --   override it but can never remove it. The list MUST NOT include duplicated
    --   parameters. A unique parameter is defined by a combination of a name and
    --   location. The list can use the Reference Object to link to parameters that
    --   are defined at the OpenAPI Object's components/parameters.
  , requestBody :: Maybe (Either ReferenceObject RequestBodyObject)
    -- ^ The request body applicable for this operation. The requestBody is only
    --   supported in HTTP methods where the HTTP 1.1 specification RFC7231 has
    --   explicitly defined semantics for request bodies. In other cases where the
    --   HTTP spec is vague, requestBody SHALL be ignored by consumers.
  , responses :: ResponsesObject
    -- ^ The list of possible responses as they are returned from executing
    --   this operation
  , callbacks :: Maybe (ReferenceOr CallbackObject)
    -- ^ A map of possible out-of band callbacks related to the parent operation.
    --   The key is a unique identifier for the Callback Object. Each value in the
    --   map is a Callback Object that describes a request that may be initiated by
    --   the API provider and the expected responses.
  , deprecated :: Maybe Bool
    -- ^ Declares this operation to be deprecated. Consumers SHOULD refrain from
    --   usage of the declared operation. Default value is false.
  , security :: Maybe [SecurityRequirementObject]
    -- ^ A declaration of which security mechanisms can be used for this
    --   operation. The list of values includes alternative security requirement
    --   objects that can be used. Only one of the security requirement objects
    --   need to be satisfied to authorize a request. To make security optional,
    --   an empty security requirement ({}) can be included in the array. This
    --   definition overrides any declared top-level security. To remove a
    --   top-level security declaration, an empty array can be used.
  , servers :: Maybe [ServerObject]
    -- ^ An alternative server array to service this operation. If an alternative
    --   server object is specified at the Path Item Object or Root level, it will
    --   be overridden by this value.
  }
  deriving stock (Generic)

data ComponentsObject = ComponentsObject
  { schemas :: [ReferenceOr SchemaObject]
    -- ^ An object to hold reusable Schema Objects
  , responses :: [ReferenceOr ResponseObject]
    -- ^ An object to hold reusable Response Objects
  , parameters :: [ReferenceOr ParameterObject]
    -- ^ An object to hold reusable Parameter Objects
  , examples :: [ReferenceOr ExampleObject]
    -- ^ An object to hold reusable Example Objects
  , requestBodies :: [ReferenceOr RequestBodyObject]
    -- ^ An object to hold reusable Request Body Objects
  , headers :: [ReferenceOr HeaderObject]
    -- ^ An object to hold reusable Header Objects
  , securitySchemes :: [ReferenceOr SecuritySchemeObject]
    -- ^ An object to hold reusable Security Scheme Objects
  , links :: [ReferenceOr LinkObject]
    -- ^ An object to hold reusable Link Objects
  , callbacks :: [ReferenceOr CallbackObject]
    -- ^ An object to hold reusable Callback Objects
  }
  deriving stock (Generic)

-- | An object containing a @$ref@ field
data ReferenceObject = ReferenceObject { ref :: Text }
  deriving stock (Generic)

data ReferenceOr a
  = Reference Text ReferenceObject
  | Or Text a
  deriving stock (Generic)

data ResponseObject = ResponseObject
  { description :: Text
    -- ^ A short description of the response. CommonMark syntax MAY be used for
    --   rich text representation.
  , headers :: Maybe [ReferenceOr HeaderObject]
    -- ^ Maps a header name to its definition. RFC7230 states header names are
    --   case insensitive. If a response header is defined with the name
    --   "Content-Type", it SHALL be ignored.
  , content :: Maybe (Map Text MediaTypeObject)
    -- ^ A map containing descriptions of potential response payloads. The key is
    --   a media type or media type range and the value describes it. For responses
    --   that match multiple keys, only the most specific key is applicable. e.g.
    --   text/plain overrides text/*
  , links :: Maybe [ReferenceOr LinkObject]
    -- ^ A map of operations links that can be followed from the response. The
    --   key of the map is a short name for the link, following the naming
    --   constraints of the names for Component Objects.
  }
  deriving stock (Generic)

data ParameterObject = ParameterObject
  { name :: Text
    -- ^ The name of the parameter. Parameter names are case sensitive.
    --
    --   If in is "path", the name field MUST correspond to a template
    --   expression occurring within the path field in the Paths Object. See
    --   Path Templating for further information.
    --
    --   If in is "header" and the name field is "Accept", "Content-Type" or
    --   "Authorization", the parameter definition SHALL be ignored.
    --
    --   For all other cases, the name corresponds to the parameter name used
    --   by the in property.
  , in_ :: ParameterIn
    -- ^ REQUIRED. The location of the parameter
  , description :: Maybe Text
    -- ^ A brief description of the parameter. This could contain examples of
    --   use. CommonMark syntax MAY be used for rich text representation.
  , required :: Maybe Bool
    -- ^ Determines whether this parameter is mandatory. If the parameter
    --   location is "path", this property is REQUIRED and its value MUST be
    --   true. Otherwise, the property MAY be included and its default value is
    --   false.
  , deprecated :: Maybe Bool
    -- ^ Specifies that a parameter is deprecated and SHOULD be transitioned
    --   out of usage. Default value is false.
  , allowEmptyValue :: Maybe Bool
    -- ^ Sets the ability to pass empty-valued parameters. This is valid only for
    --   query parameters and allows sending a parameter with an empty value.
    --   Default value is false. If style is used, and if behavior is n/a (cannot
    --   be serialized), the value of allowEmptyValue SHALL be ignored. Use of this
    --   property is NOT RECOMMENDED, as it is likely to be removed in a later
    --   revision.
  , style :: Maybe StyleValue
    -- ^ Describes how the parameter value will be serialized depending on the
    --   type of the parameter value. Default values (based on value of in):
    --   for @query - form@; for @path - simple@; for @header - simple@; for
    --   @cookie - form@.
  , explode :: Maybe Bool
    -- ^ When this is true, parameter values of type array or object generate
    --   separate parameters for each value of the array or key-value pair of the
    --   map. For other types of parameters this property has no effect. When
    --   style is form, the default value is true. For all other styles, the
    --   default value is false.
  , allowReserved :: Maybe Bool
    -- ^ Determines whether the parameter value SHOULD allow reserved characters,
    --   as defined by RFC3986 @:/?#[]@!$&'()*+,;=@ to be included without
    --   percent-encoding. This property only applies to parameters with an in
    --   value of query. The default value is false.
  , schema :: Maybe (Either ReferenceObject SchemaObject)
    -- ^ The schema defining the type used for the parameter.
  , example :: Maybe Aeson.Value
    -- ^ Example of the parameter's potential value. The example SHOULD match
    --   the specified schema and encoding properties if present. The example
    --   field is mutually exclusive of the examples field. Furthermore, if
    --   referencing a schema that contains an example, the example value SHALL
    --   override the example provided by the schema. To represent examples of
    --   media types that cannot naturally be represented in JSON or YAML, a
    --   string value can contain the example with escaping where necessary.
  , examples :: Maybe [ReferenceOr ExampleObject]
  -- ^ Examples of the parameter's potential value. Each example SHOULD contain
  --   a value in the correct format as specified in the parameter encoding.
  --   The examples field is mutually exclusive of the example field.
  --   Furthermore, if referencing a schema that contains an example, the
  --   examples value SHALL override the example provided by the schema.
  , content :: Maybe (Map Text MediaTypeObject)
  -- ^ A map containing the representations for the parameter. The key is the
  --   media type and the value describes it. The map MUST only contain one
  --   entry.
  }
  deriving stock (Generic)

-- | The @in@ field as an enum
data ParameterIn
  = Query
  | Header
  | Path
  | Cookie
  deriving stock (Generic)

data StyleValue
  = Matrix
    -- ^ Path-style parameters defined by RFC6570
  | Label
    -- ^ Label style parameters defined by RFC6570
  | Form
    -- ^ Form style parameters defined by RFC6570. This option replaces
    --   collectionFormat with a csv (when explode is false) or multi (when
    --   explode is true) value from OpenAPI 2.0.
  | Simple
    -- ^ Simple style parameters defined by RFC6570. This option replaces
    --   collectionFormat with a csv value from OpenAPI 2.0.
  | SpaceDelimited
    -- ^ Space separated array values. This option replaces collectionFormat
    --   equal to ssv from OpenAPI 2.0.
  | PipeDelimited
    -- ^ Pipe separated array values. This option replaces collectionFormat
    --   equal to pipes from OpenAPI 2.0.
  | DeepObject
    -- ^ Provides a simple way of rendering nested objects using form
    --   parameters.
  deriving stock (Generic)

data ExampleObject = ExampleObject
  { summary :: Maybe Text
    -- ^ Short description for the example
  , description :: Maybe Text
    -- ^ Long description for the example
    --
    --   CommonMark syntax MAY be used for rich text representation.
  , value :: Maybe Aeson.Value
    -- ^ Embedded literal example
    --
    --   The 'value' field and 'externalValue' field are mutually exclusive. To
    --   represent examples of media types that cannot naturally represented in
    --   JSON or YAML, use a string value to contain the example, escaping
    --   where necessary.
  , externalValue :: Maybe Text
    -- ^ A URL that points to the literal example
    --
    --   This provides the capability to reference examples that cannot easily
    --   be included in JSON or YAML documents. The 'value' field and
    --   'externalValue' field are mutually exclusive.
  }
  deriving stock (Generic)

data RequestBodyObject = RequestBodyObject
  { description :: Maybe Text
    -- ^ A brief description of the request body. This could contain examples
    --   of use. CommonMark syntax MAY be used for rich text representation.
  , content :: Map Text MediaTypeObject
    -- ^ The content of the request body. The key is a media type or media type
    --   range and the value describes it. For requests that match multiple keys,
    --   only the most specific key is applicable. e.g. text/plain overrides
    --   @text/*@
  , required :: Bool
    -- ^ Determines if the request body is required in the request. Defaults to
    --   false
  }
  deriving stock (Generic)

data HeaderObject = HeaderObject
  { description :: Maybe Text
    -- ^ A brief description of the parameter. This could contain examples of
    --   use. CommonMark syntax MAY be used for rich text representation.
  , required :: Maybe Bool
    -- ^ Determines whether this parameter is mandatory. If the parameter
    --   location is "path", this property is REQUIRED and its value MUST be
    --   true. Otherwise, the property MAY be included and its default value is
    --   false.
  , deprecated :: Maybe Bool
    -- ^ Specifies that a parameter is deprecated and SHOULD be transitioned
    --   out of usage. Default value is false.
  , explode :: Maybe Bool
    -- ^ When this is true, parameter values of type array or object generate
    --   separate parameters for each value of the array or key-value pair of the
    --   map. For other types of parameters this property has no effect. The
    --   default value is false.
  , schema :: Maybe (Either ReferenceObject SchemaObject)
    -- ^ The schema defining the type used for the parameter.
  , example :: Maybe Aeson.Value
    -- ^ Example of the parameter's potential value. The example SHOULD match
    --   the specified schema and encoding properties if present. The example
    --   field is mutually exclusive of the examples field. Furthermore, if
    --   referencing a schema that contains an example, the example value SHALL
    --   override the example provided by the schema. To represent examples of
    --   media types that cannot naturally be represented in JSON or YAML, a
    --   string value can contain the example with escaping where necessary.
  , examples :: Maybe [ReferenceOr ExampleObject]
  -- ^ Examples of the parameter's potential value. Each example SHOULD contain
  --   a value in the correct format as specified in the parameter encoding.
  --   The examples field is mutually exclusive of the example field.
  --   Furthermore, if referencing a schema that contains an example, the
  --   examples value SHALL override the example provided by the schema.
  }
  deriving stock (Generic)

data SecuritySchemeObject = SecuritySchemeObject
  { type_ :: SecuritySchemaType
  }
  deriving stock (Generic)

newtype SecuritySchemaType = SecuritySchemaType Text
  deriving newtype (IsString)

typeApiKey :: SecuritySchemaType
typeApiKey = "apiKey"

-- FIXME
data LinkObject
  deriving stock (Generic)

-- FIXME
data CallbackObject
  deriving stock (Generic)

-- FIXME
data SchemaObject
  deriving stock (Generic)

-- FIXME
data MediaTypeObject
  deriving stock (Generic)

-- FIXME
data ResponsesObject
  deriving stock (Generic)

