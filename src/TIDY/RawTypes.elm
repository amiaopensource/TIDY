module TIDY.RawTypes where

import Array exposing (Array)
import Json.Decode exposing (..)

type alias ClusterChoice =
  { v : String
  , c : Int
  }

type alias Cluster =
  { edit : Bool
  , choices : Array ClusterChoice
  , value : String
  , size : Int
  , rowCount : Int
  , avg : Float
  , variance : Float
  }

type alias ClusterFile =
  { projectName : String
  , columnName : String
  , timeStamp : String
  , clusterMethod : String
  , keyingFunction : String
  , clusters : Array Cluster
  }

decodeCluster : Decoder Cluster
decodeCluster =
  object7 Cluster
    ("edit" := bool)
    ("choices" := array (object2 ClusterChoice ("v" := string) ("c" := int)))
    ("value" := string)
    ("size" := int)
    ("rowCount" := int)
    ("avg" := float)
    ("variance" := float)

decodeClusterFile : Decoder ClusterFile
decodeClusterFile =
  object6 ClusterFile
    ("projectName" := string)
    ("columnName" := string)
    ("timeStamp" := string)
    ("clusterMethod" := string)
    ("keyingFunction" := string)
    ("clusters" := array decodeCluster)
