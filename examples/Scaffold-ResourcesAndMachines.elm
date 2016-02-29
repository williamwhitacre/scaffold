{- Boilerplate for getting started using Scaffold.App -}

import Scaffold.App as App
import Scaffold.Machine as Machine exposing (Machine)
import Scaffold.Resource as Resource exposing (Resource, ResourceTask)


import Html exposing (Html, div, span, button, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)


import Signal
import Task exposing (Task)


import Time exposing (Time)


import Collection


-- Our action type.
type Action =
  NoOp


type alias Model =
  { data : Collection () String String
  }


-- We are using Html for the output given that the Layout module relies on the now depreciated
-- Graphics.Element symachine. Layout is rewritten to use Html internally in the updated Scaffold
-- package.
styleOut : Html.Attribute
styleOut =
  style
    [ ("position", "absolute")
    , ("top", "20px")
    , ("left", "20px")
    , ("right", "20px")
    , ("bottom", "20px")
    ]


-- Initial model.
model0 : Model
model0 =
  { data = 0    -- Initial value set to 0.
  }


-- Present the model.
present : Signal.Address (List Action) -> Time -> Model -> App.ViewOutput Action Html ()
present address now model =
  div
    [ styleOut ]
    [ div
        [ ]
        [ text "Clean slate."
        ]
    ]

  -- Make a ViewOutput
  |> App.presented


-- Update the model. Nothing unfamiliar here.
update : Action -> Time -> Model -> App.UpdatedModel Action Model ()
update action now model =
  case action of
    NoOp -> App.updated model


-- Set up the program.
output : App.ProgramOutput Action Model Html ()
output =
  App.defProgram present update model0 -- Define your top level program. (defProgram' to use staging.)
  |> App.run     -- Invoke the configured ProgramInput to get a ProgramOutput
  |> App.itself  -- Wire task output back in.


main : Signal Html
main = App.outputView output -- output the view


-- Use sink to run the concrete task output of the program.
port sink : Signal (Task z ())
port sink = App.sink output
