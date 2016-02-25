{-

EXAMPLE 0: How to use the basic features of Core in one example. Makes a counter with an
automatic mode that uses the OrbiterTask utilities, and sets up a basic Orbiter. The architectural
weight needed to initially set up an Orbiter is a bit heavier than that which you'd see in a
StartApp application. The added flexibility of the Orbiter is visible here, but not very heavily
excercised. NOTE also that this does not use the more flexible `defProgram'` which provides stage.
This is appropriate for any endpoint simple controls that do not manage collections of state, and
mirrors the structure of StartApp in this way.

-}

import Gigan.Core exposing (..)

-- We are using HTML here to avoid introducing too many unfamiliar things at once.
import Html exposing (Html, div, span, button, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)

-- We'll keep track of changes in the Window size.
import Window

import Signal
import Task exposing (Task)

import Time exposing (Time)


-- Our action type.
type Action =
  Change Int          -- change the value of the counter by some amount.
  | AutoOn Int        -- turn on auto change with some amount.
  | AutoDo            -- apply the current auto amount.
  | AutoOff           -- turn off auto.
  | CurrentWidth Int  -- updated the current view width.


type alias Model =
  { stop : Bool   -- set to true when user wants auto to stop.
  , auto : Int    -- set to the current auto change amount, or 0 if auto is off.
  , value : Int   -- the current value of the counter.
  , width : Int   -- the browser client width at the moment.
  }


-- This function produces a TaskDispatchment from the current model which either has no tasks to
-- run in the case that the auto mode is not enabled, otherwise it produces an OrbiterTask using
-- the blindOrbiterAgent, which invokes the AutoDo action after sleeping for one second.
autoIncrement : Model -> TaskDispatchment () Action
autoIncrement model =
  if not model.stop && model.auto /= 0 then
    -- we are in auto mode with no user request to stop dispatch one task that always executes
    -- AutoDo exactly one second in to the future.
    dispatchTasks [orbiterBlindAgent (orbiterAgentSuccess [AutoDo]) (Task.sleep 1000)]
  else
    -- do nothing because we are in auto mode. NOTE: an empty task dispatchment will not result in
    -- the execution of any tasks, not even a do-nothing task.
    dispatchTasks []


-- We are using Html for the output given that the Layout module relies on the now depreciated
-- Graphics.Element system. Layout is rewritten to use Html internally in the updated Scaffold
-- package.
styleOut : Html.Attribute
styleOut =
  style
    [ ("position", "absolute")
    , ("top", "20px")
    , ("left", "20px")
    ]


-- Initial model.
model0 : Model
model0 =
  { stop = False -- Stop command not yet given, initial state.
  , auto = 0     -- No auto counter.
  , value = 0    -- Initial value set to 0.
  , width = 0    -- No initial width.
  }


-- Select which action to send on an "auto" button press, establishing a toggle.
autoActions : Model -> Int -> List Action
autoActions model amount =
  [if model.auto == 0 then AutoOn amount else AutoOff]


-- Select the button test based on said toggle state.
stopOr : Model -> String -> String
stopOr model say =
  if model.auto /= 0 && not model.stop then "stop" else say


-- Present the model.
present : Signal.Address (List Action) -> Time -> Model -> ViewOutput Action Html ()
present address now model =
  div
    [ styleOut ]
    [ div
        [ ]
        [ button [ onClick address (autoActions model -1) ] [ text (stopOr model "auto -") ]
        , button [ onClick address [Change -1] ]            [ text "-" ]
        , button [ onClick address [Change 1] ]             [ text "+" ]
        , button [ onClick address (autoActions model 1) ]  [ text (stopOr model "auto +") ]

        , span [] [ text (toString model.value) ]
        ]
    , div
        [ ]
        [ Html.hr [] []
        , text ((++) "window width last changed to " <| toString model.width)
        , Html.br [] []
        , text ((++) "the timestamp of the last executed action was " <| toString now)
        ]
    ]

  |> presented


-- Update the model. Nothing unfamiliar here.
update : Action -> Time -> Model -> UpdatedModel Action Model ()
update action now model =
  case action of
    CurrentWidth width ->
      { model | width = width }
      |> updated

    Change amount ->
      { model | value = model.value + amount }
      |> updated

    AutoOn amount ->
      { model | stop = False, auto = amount }
      |> \model' -> updated model'
      |> withDispatchment (autoIncrement model')

    AutoDo ->
      update (Change model.auto) now model
      |> withDispatchment (autoIncrement model)

    AutoOff ->
      { model | stop = True, auto = 0 }
      |> updated


-- Set up the orbiter.
output : OrbiterOutput Action Model Html ()
output =
  (defProgram present update model0)
    `withSequenceInputs`
      [ Signal.map (CurrentWidth >> flip (::) []) Window.width ]
    `orbitsWithWork`
      nilTask
    +--> itself


-- Output root view.
main : Signal Html.Html
main = output.view'


-- Use sieve to run the concrete task output of the orbiter.
port sink : Signal (Task z ())
port sink = sieve output
