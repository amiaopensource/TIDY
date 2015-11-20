module TIDY where

import Array exposing (Array)
import Effects exposing (Effects, Never)
import Html exposing (div, span, button, text, input)
import Html.Attributes exposing (placeholder, value, class)
import Html.Events exposing (onClick, on, targetValue)
import Http
import Maybe exposing (withDefault)
import Task

import TIDY.RawTypes exposing (Cluster, ClusterFile, decodeClusterFile)

type alias Model =
  { clusters : Array Cluster
  , currentIndex : Int
  , proposedValue : String
  }

type Action = LoadClusters (Maybe ClusterFile)
            | AcceptCluster Cluster
            | PreviousCluster
            | NextCluster
            | StartFromBeginning
            | SetProposedString String

initModel : Model
initModel =
  { clusters = Array.empty
  , currentIndex = 0
  , proposedValue = ""
  }

initEffects : Effects Action
initEffects =
  Http.get decodeClusterFile (Http.url "/clusters_sample.json" [])
    |> Task.toMaybe
    |> Task.map LoadClusters
    |> Effects.task

init = (initModel, initEffects)

updateIndex model i =
  { model | currentIndex = i }

update action model =
  case action of
    LoadClusters maybeCf ->
      case maybeCf of
        Just clusterFile ->
          ({model | clusters = clusterFile.clusters}, Effects.none)
        Nothing ->
          (model, Effects.none)
    PreviousCluster ->
      (updateIndex model (model.currentIndex - 1), Effects.none)
    NextCluster ->
      (updateIndex model (model.currentIndex + 1), Effects.none)
    StartFromBeginning ->
      (updateIndex model 0, Effects.none)
    AcceptCluster cluster ->
      (model, Effects.none)
    SetProposedString str ->
      ({model | proposedValue = str}, Effects.none)

viewClusterChoice address cc =
  div []
    [ span [] [ text cc.v ]
    , span [] [ text ("(" ++ (toString cc.c) ++ ")")]
    ]

viewCluster address cluster =
  div []
    (Array.toList <| Array.map (viewClusterChoice address) cluster.choices)

stringInput address string =
  input
    [ placeholder "Corrected value"
    , value string
    , on "input" targetValue (Signal.message address << SetProposedString)
    ]
  []

view address model =
  let maybeCluster = Array.get model.currentIndex model.clusters
      accept = Maybe.map (AcceptCluster) maybeCluster |>
               withDefault StartFromBeginning
  in div [ class "container" ]
       [ div [ class "row" ]
         [ div [ class "col-xs-12" ]
           [ div [] [ text "Select the correct one:" ]
           , withDefault (div [] []) <| Maybe.map (viewCluster address) maybeCluster
       -- , div [] [ text (toString model) ]
           , stringInput address (model.proposedValue)
           , div [] [ button [ onClick address PreviousCluster ] [ text "Back" ]
                    , button [ onClick address NextCluster ] [ text "Next" ]
                    ]
           ]
         ]
       ]
