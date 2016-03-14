{-

How to use the basic features of App in one example. Makes a counter with an
automatic mode that uses the ProgramTask utilities, and sets up a basic Program. The architectural
weight needed to initially set up an Program is a bit heavier than that which you'd see in a
StartApp application. The added flexibility of the Program is visible here, but not very heavily
excercised. NOTE also that this does not use the more flexible `defProgram'` which provides stage.
This is appropriate for any endpoint simple controls that do not manage collections of state, and
mirrors the structure of StartApp in this way.

-}


import Scaffold.App as App
import Scaffold.Layout as Layout


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
  Change Int                 -- change the value of the counter by some amount.
  | AutoOn Int               -- turn on auto change with some amount.
  | AutoDo                   -- apply the current auto amount.
  | AutoOff                  -- turn off auto.
  | SetClientSize (Int, Int) -- client size updated.


type alias Model =
  { stop : Bool   -- set to true when user wants auto to stop.
  , auto : Int    -- set to the current auto change amount, or 0 if auto is off.
  , value : Int   -- the current value of the counter.
  , width : Int   -- the browser client width at the moment.
  , height : Int   -- the browser client width at the moment.
  }


-- This function produces a TaskDispatchment from the current model which either has no tasks to
-- run in the case that the auto mode is not enabled, otherwise it produces an ProgramTask using
-- the blindProgramAgent, which invokes the AutoDo action after sleeping for one second.
autoIncrement : Model -> App.TaskDispatchment () Action
autoIncrement model =
  if not model.stop && model.auto /= 0 then
    -- we are in auto mode with no user request to stop dispatch one task that always executes
    -- AutoDo exactly one second in to the future.
    App.dispatchTasks [App.blindAgent (App.agentSuccess [AutoDo]) (Task.sleep 1000)]
  else
    -- do nothing because we are in auto mode. NOTE: an empty task dispatchment will not result in
    -- the execution of any tasks, not even a do-nothing task.
    App.dispatchTasks []


-- Initial model.
model0 : Model
model0 =
  { stop = False -- Stop command not yet given, initial state.
  , auto = 0     -- No auto counter.
  , value = 0    -- Initial value set to 0.
  , width = 0    -- No initial width.
  , height = 0   -- No initial height.
  }


-- Select which action to send on an "auto" button press, establishing a toggle.
autoActions : Model -> Int -> List Action
autoActions model amount =
  [ if model.auto == 0 then AutoOn amount else AutoOff ]


-- Select the button test based on said toggle state.
stopOr : Model -> String -> String
stopOr model say =
  if model.auto /= 0 && not model.stop then "stop" else say


-- Present the model.
present : Signal.Address (List Action) -> Time -> Model -> App.ViewOutput Action Layout.Item ()
present address now model =
  Layout.group
    [ Layout.makeItem 350 200 div
        [ ]
        [ text ((++) "window size last changed to " <| toString (model.width, model.height))
        , Html.hr [] []
        , text ((++) "the timestamp of the last executed action was " <| toString now)
        ]
    , Layout.makeItem 350 20 div
        [ ]
        [ button [ onClick address (autoActions model -1) ] [ text (stopOr model "auto -") ]
        , button [ onClick address [Change -1] ]            [ text "-" ]
        , button [ onClick address [Change 1] ]             [ text "+" ]
        , button [ onClick address (autoActions model 1) ]  [ text (stopOr model "auto +") ]

        , span [] [ text (toString model.value) ]
        ]
    ]

  |> Layout.thenDo (Layout.grabItemRelativeX 0.5)
  |> Layout.thenDo (Layout.snapToRule <| Layout.verticalRuleCentered 0 model.width)
  |> Layout.spacedBetween Layout.vertically (model.height // 4) (model.height * 3 // 4)

  |> Layout.flattenTo (Layout.defBounds 0 0 model.width model.height)

  |> App.presented


-- Update the model. Nothing unfamiliar here.
update : Action -> Time -> Model -> App.UpdatedModel Action Model ()
update action now model =
  case action of
    SetClientSize (w, h) ->
      { model | width = w, height = h }
      |> App.updated

    Change amount ->
      { model | value = model.value + amount }
      |> App.updated

    AutoOn amount ->
      { model | stop = False, auto = amount }
      |> \model' -> App.updated model'
      |> App.withDispatchment (autoIncrement model')

    AutoDo ->
      update (Change model.auto) now model
      |> App.withDispatchment (autoIncrement model)

    AutoOff ->
      { model | stop = True, auto = 0 }
      |> App.updated



clientSizeInput : Signal (List Action)
clientSizeInput =
  Signal.map (SetClientSize >> flip (::) []) Window.dimensions


-- Set up the program.
output : App.ProgramOutput Action Model Layout.Item ()
output =
  App.defProgram present update model0
  |> App.defSequenceInputs [ clientSizeInput ]
  |> App.run     -- ProgramInput -> ProgramOutput
  |> App.itself  -- Wire task output back in.


-- Output root view.
main : Signal Html.Html
main =
  App.outputView output
  |> Signal.map Layout.fromItem


-- Use sink to run the concrete task output of the program.
port sink : Signal (Task z ())
port sink = App.sink output
