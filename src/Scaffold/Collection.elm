module Scaffold.Collection where


import Scaffold.Resource as Resource exposing (Resource, ResourceTask)


type alias Path comparable = List comparable


type DataType euser comparable v =
  Datum (Time, Resource user v)
  | Data (Collection euser comparable v)


{-| Fully represents a bit of data and it's location. This might be another collection or a leaf. -}
type alias Container euser comparable v =
  { resource : DataType euser comparable v
  , path : Path comparable
  }


type alias Collection euser comparable v =
  { data : Dict comparable (Container euser comparable v)
  , deltaOut : Dict comparable (Resource euser v)
  }
