module TIDY where

import Array exposing (Array)
import Effects exposing (Effects, Never)
import Html exposing (div, span, button, text, input, h1, h2, small)
import Html.Attributes exposing (placeholder, class, disabled)
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
  Http.get decodeClusterFile (Http.url "clusters_sample.json" [])
    |> Task.toMaybe
    |> Task.map LoadClusters
    |> Effects.task

init = (initModel, initEffects)

updateIndex model i =
  let propV = Array.get i model.clusters |>
              Maybe.map (\x -> x.value) |>
              withDefault ""
  in { model | currentIndex = if i < 0 then 0 else i, proposedValue = propV }

update action model =
  case action of
    LoadClusters maybeCf ->
      case maybeCf of
        Just clusterFile ->
          ({model | clusters = Array.map (\x -> {x | value = ""}) clusterFile.clusters}, Effects.none)
        Nothing ->
          (model, Effects.none)
    PreviousCluster ->
      (updateIndex model (model.currentIndex - 1), Effects.none)
    NextCluster ->
      (updateIndex model (model.currentIndex + 1), Effects.none)
    StartFromBeginning ->
      (updateIndex model 0, Effects.none)
    AcceptCluster cluster ->
      let accepted = {cluster | edit = True, value = model.proposedValue}
          updatedClusters = Array.set model.currentIndex accepted model.clusters
          model0 = {model | clusters = updatedClusters}
      in ((updateIndex model0 (model0.currentIndex + 1)), Effects.none)
    SetProposedString str ->
      ({model | proposedValue = str}, Effects.none)

viewClusterChoice address model cc =
  let selected = cc.v == model.proposedValue
  in div [ onClick address (SetProposedString cc.v)
         , class (if selected then "bg-success" else "")]
       [ h2 [ class "p-y" ] [ text cc.v
               , small [ class "text-muted"]
                  [ text (" (" ++ (toString cc.c) ++ ")") ]
               ]
       ]

viewCluster address model cluster =
  div []
      -- [text (toString cluster)]
    (Array.toList <| Array.map (viewClusterChoice address model) cluster.choices)

stringInput address string =
  input
    [ placeholder "Corrected value"
    , Html.Attributes.value string
    , Html.Attributes.type' "text"
    , class "form-control"
    , on "input" targetValue (Signal.message address << SetProposedString)
    ]
  []

btnClass x = class <| "btn btn-" ++ x ++ " btn-lg btn-block"

view address model =
  let maybeCluster = Array.get model.currentIndex model.clusters
      accept = Maybe.map (AcceptCluster) maybeCluster |>
               withDefault StartFromBeginning
      canAccept = model.proposedValue /= ""
  in div [ class "container-fluid" ]
       [ div [ class "row" ]
         [ div [ class "col-xs-12" ]
           [ h1 [ class "display-1 text-muted" ] [ text "Select the correct one:" ]
           , withDefault (div [] []) <| Maybe.map (viewCluster address model) maybeCluster
           ]
         ]
       , div [ class "row" ]
           [ div [ class "col-xs-12" ] [ stringInput address (model.proposedValue) ] ]
       , div [ class "row" ]
           [ div [ class "col-xs-4" ]
             [ button [ onClick address PreviousCluster
                      , btnClass "secondary"
                      , disabled False
                      ] [ text "Back" ] ]
           , div [ class "col-xs-4" ]
               [ button [ onClick address NextCluster
                        , btnClass "warning"
                        , disabled False
                        ] [ text "Skip" ] ]
           , div [ class "col-xs-4" ]
               [ button [ onClick address accept
                        , btnClass (if canAccept then "success" else "secondary")
                        , disabled (not canAccept) ] [ text "Accept" ] ]
           ]
       ]
