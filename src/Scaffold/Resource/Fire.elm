module Scaffold.Resource.Fire

  (Config, Output, Action(..), Machine, config, withOrdering, program, machine)

  where


{-| Firebase bindings for Scaffold.Resource using ElmFire.Dict and ElmFire.Op

@docs Config, Output, Action, Machine, config, withOrdering, program, machine
-}

import Scaffold.App as App
import Scaffold.Machine as Machine
import Scaffold.Resource as Res
import Scaffold.Error as Error


import Time exposing (Time)
import Task exposing (Task, andThen, onError)


import ElmFire
import ElmFire.Auth
import ElmFire.Dict
import ElmFire.Op


import Dict exposing (Dict)


import Json.Encode
import Json.Decode


{-| Convenience type for configuration -}
type alias Config v =
  { location : ElmFire.Location
  , orderOptions : ElmFire.OrderOptions
  , encoder : v -> Json.Encode.Value
  , decoder : Json.Decode.Decoder v
  }


{-| -}
config : (v -> Json.Encode.Value) -> (Json.Decode.Decoder v) -> ElmFire.Location -> Config v
config encoder decoder location =
  { location = location
  , encoder = encoder
  , decoder = decoder
  , orderOptions = ElmFire.noOrder
  }


{-| -}
withOrdering : ElmFire.OrderOptions -> Config v -> Config v
withOrdering orderOptions config =
  { config
  | orderOptions = orderOptions
  }


{-| Machine output type. -}
type alias Output v =
  { resource : Res.Resource String v
  , location : ElmFire.Location
  , isRunning : Bool
  , lastUpdated : Time
  }


{-| Machine type. -}
type alias Machine v =
  Machine.Machine (Action v) (Model v) (Output v) ElmFire.Error


{-| Dataset action type -}
type Action v =
  Reconfigure (Config v)
  | Kill
  | Started (Task ElmFire.Error ())
  | ApplyDelta (ElmFire.Dict.Delta v)
  | DoOperation (ElmFire.Op.Operation v)
  | ReportError ElmFire.Error


type alias Model v =
  { output : Output v
  , elmfireConfig : ElmFire.Dict.Config v -- Internal configuration.
  , config : Config v
  , isStarted : Bool
  , resource : Res.Resource String v
  , kill : Maybe (Task ElmFire.Error ())
  }


elmfireConfigOf : Config v -> ElmFire.Dict.Config v
elmfireConfigOf config =
  { location = config.location
  , orderOptions = config.orderOptions
  , encoder = config.encoder
  , decoder = config.decoder
  }


initialOutput : Config v -> Output v
initialOutput config =
  { resource = Res.groupResource []
  , location = config.location
  , isRunning = False
  , lastUpdated = 0.0
  }


initialModel : Config v -> Model v
initialModel config =
  { output = initialOutput config
  , elmfireConfig = elmfireConfigOf config
  , config = config
  , resource = Res.groupResource []
  , isStarted = False
  , kill = Nothing
  }


updateOutput : Time -> Model v -> Model v
updateOutput now model =
  model.output
  |> \modelOutput -> { modelOutput
  | isRunning =
    case model.kill of
      Nothing -> False
      Just _ -> True
  , lastUpdated = now
  , resource = model.resource
  } |> \output' -> { model | output = output' }


--tryKill : Time -> Model v -> (Model v, List (App.ProgramTask z Action))
tryKill now model =
  case model.kill of
    Nothing -> (model, [ ])
    Just killTask ->
      killTask
      |> App.failureAgent
          (App.agentSuccess [])
          (ReportError >> App.agentSingletonSuccess)
      |> \outTask -> ({ model | kill = Nothing }, [ outTask ])


update : Action v -> Time -> Model v -> App.UpdatedModel (Action v) (Model v) ElmFire.Error
update action now model =
  case (Debug.log "Fire action" action) of
    Reconfigure newConfig ->
      let
        (model', tasks) = tryKill now model
      in
        App.updated { model' | config = newConfig, isStarted = False }
        |> App.withTasks tasks

    Started killTask ->
      { model | kill = Just killTask }
      |> App.updated

    Kill ->
      let
        (model', tasks) = tryKill now model
      in
        App.updated model
        |> App.withTasks tasks

    ApplyDelta delta ->
      case model.kill of
        Nothing -> App.updated model
        Just _ ->
          (case delta of
            ElmFire.Dict.Idem -> model

            ElmFire.Dict.Added key value ->
              { model
              | resource = Res.writePath [key] (Res.defResource value) model.resource
              }

            ElmFire.Dict.Changed key value ->
              { model
              | resource = Res.writePath [key] (Res.defResource value) model.resource
              }

            ElmFire.Dict.Removed key value ->
              { model
              | resource = Res.deletePath [key] model.resource
              }

            ElmFire.Dict.Undecodable key reason ->
              { model
              | resource = Res.writePath [key] (Res.undecidedResource (Error.unknownError reason)) model.resource
              }

            ElmFire.Dict.Unsubscribed ->
              { model
              | resource = Res.unknownResource
              }

            ElmFire.Dict.QueryError error ->
              { model
              | resource = Res.undecidedResource (Error.unknownError error.description)
              }
          ) |> App.updated


    DoOperation op ->
      case model.kill of
        Nothing -> App.updated model
        Just _ ->
          App.updated model
          |> App.withTask
            (ElmFire.Op.operate model.elmfireConfig op
            |> App.blindAgent (App.agentSuccess []))

    ReportError error ->
      { model
      | resource = Res.undecidedResource (Error.unknownError error.description)
      } |> App.updated




subscribe : Signal.Address (List (Action v)) -> Time -> Model v -> App.UpdatedModel (Action v) (Model v) ElmFire.Error
subscribe address now model =
  let
    subscriptionTask =
      ElmFire.Dict.subscribeDelta (Signal.forwardTo (App.forwardSingleton address) ApplyDelta) model.elmfireConfig
      |> App.agent
        (\killTask -> Debug.log "started Fire subscription" () |> \_ -> App.agentSingletonSuccess (Started killTask))
        (\error -> Debug.log "error starting Fire subscription" () |> \_ -> App.agentSingletonSuccess (ReportError error))

  in
    { model | isStarted = True }
    |> updateOutput now
    |> App.updated
    |> App.withTasks [subscriptionTask]


stage : Signal.Address (List (Action v)) -> Time -> Model v -> App.UpdatedModel (Action v) (Model v) ElmFire.Error
stage address now model =
  if not model.isStarted then
    subscribe address now model
  else
    updateOutput now model
    |> App.updated


present : Signal.Address (List (Action v)) -> Time -> Model v -> App.ViewOutput (Action v) (Output v) ElmFire.Error
present address now model =
  App.presented model.output


{-| -}
program : Config v -> App.ProgramInput (Action v) (Model v) (Output v) ElmFire.Error
program = initialModel >> App.defStagedProgram present stage update


{-| -}
machine : Config v -> Machine v
machine = program >> Machine.machine
