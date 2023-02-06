module GraphQLAPI where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus (interpreter)
import Data.Morpheus.Document (importGQLDocument)
import Data.Morpheus.Types (RootResolver (..))

import Servant.Api

importGQLDocument "schema.graphql"

rootResolver :: RootResolver IO () Query () ()
rootResolver =
    RootResolver
        { queryResolver = Query{books}
        , mutationResolver = ()
        , subscriptionResolver = ()
        }
  where
    books BooksArgs{operation, encodedBooksFile} =
        pure Book{title = pure "42", author = "42a", isbn13 = "42isbn"}

api :: ByteString -> IO ByteString
api = interpreter rootResolver
