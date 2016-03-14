{- Boilerplate for getting started using Scaffold.App -}

import Scaffold.App as App

-- We are using HTML here to avoid introducing too many unfamiliar things at once.
import Html exposing (Html, div, span, button, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)

import Signal
import Task exposing (Task)

import Time exposing (Time)


-- Our action type.
type Action =
  Change Int -- change the value of the counter by some amount.

type alias Model =
  { value : Int   -- the current value of the counter.
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
  { value = 0    -- Initial value set to 0.
  }


-- Present the model.
present : Signal.Address (List Action) -> Time -> Model -> App.ViewOutput Action Html ()
present address now model =
  div
    [ styleOut ]
    [ div
        [ ]
        -- Note we dispatch actions either by sending to an address, or by producing tasks which
        -- will do this after some other computation completes in the future, as in the Elm
        -- Architecture. The only real difference is that we treat lists of actions as batched
        -- operations so that metaprogramming can be used to build on a simpler controller without
        -- modifying it, or simply for the sake of efficiency. We have to do a lot of work, it'd be
        -- worse if we also took up resources each time generating a new view as well. Lists of
        -- actions and lists of tasks are inherently more flexible because we can encode sequences
        -- without forgetting their contents.
        [ button [ onClick address [Change -1] ] [ text "(-)" ]
        , button [ onClick address [Change 1] ]  [ text "(+)" ]

        , span [] [ text (toString model.value) ]
        , Html.hr [] []
        , text ((++) "the timestamp of the last executed action was " <| toString now)
        ]
    ]

  -- Make a ViewOutput
  |> App.presented


-- Update the model. Nothing unfamiliar here.
update : Action -> Time -> Model -> App.UpdatedModel Action Model ()
update action now model =
  case action of
    Change amount ->
      { model | value = model.value + amount }
      |> App.updated


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
