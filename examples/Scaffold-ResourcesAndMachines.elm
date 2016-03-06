{- Boilerplate for getting started using Scaffold.App -}

import Scaffold.App as App
import Scaffold.Machine as Machine exposing (Machine)
import Scaffold.Resource as Resource exposing (Resource, ResourceRef, UserTask, ResourceTask)


import Html exposing (Html, div, span, button, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Html.Lazy as HL


import Signal
import Task exposing (Task)


import Time exposing (Time)


type alias TreeItem = (String, String)


-- Our action type.
type Action =
  Delta (ResourceRef () TreeItem)
  | Dispatch (ResourceTask () TreeItem)


type alias ModelResource =
  Resource () TreeItem


-- We are using Html for the output given that the Layout module relies on the now depreciated
-- Graphics.Element system. Layout is rewritten to use Html internally in the updated Scaffold
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


styleItem : Html.Attribute
styleItem =
  style
    [ ("display", "inline-block")
    ]


-- Initial model.
model0 : ModelResource
model0 = Resource.voidResource    -- Initial value set to unknown.


renderItem : Signal.Address (List Action) -> Time -> TreeItem -> Html
renderItem address now (key, datum) =
  HL.lazy2 Html.div
    [ styleItem ] -- attribs
    [ Html.h3 "\"" ++ key ++ "\""
    , Html.text datum
    ] -- html items
-- TODO: add controls!


renderUnknown : Signal.Address (List Action) -> Time -> TreeItem


-- remember that ModelResource = Resource () TreeItem
renderResource : Signal.Address (List Action) -> Time -> ModelResource -> Html
renderResource address now (key, datum)
  Resource.therefore (renderItem address now) res
  |> Resource.assumeIfNow Resource.isVoid (renderItem address now (key, "Sorry, nothing was found."))
  |> Resource.assumeIfNow Resource.isPending (renderItem address now (key, "Please wait..."))
  -- assumeInCase* family takes a function `Resource euser v -> Maybe v` which should return Nothing
  -- if no assumption is to be made in which case the resulting resource will be the argument.
  -- Otherwise Just x should be returned, where `x` is of type `v`. The latter results in a some
  -- resource `res` where `res == defResource x`.
  |> Resource.assumeInCaseNow
      (Resource.therefore Nothing >> Resource.assumeInCaseNow (renderUnknown address >> Just))
  |> Resource.otherwise (errorHtml address)

emptyHtml


pleaseWaitHtml


unknownHtml


errorHtml


presentResource : Signal.Address (List Action) -> Time -> ModelResource -> App.ViewOutput Action Html ()
presentResource address now res =
  Resource.therefore (presentItem address now) res
  |> Resource.assumeIfNow Resource.isVoid (emptyHtml address)
  |> Resource.assumeIfNow Resource.isPending pleaseWaitHtml
  -- assumeInCase* family takes a function (Resource euser v -> Maybe v) which should return Nothing
  -- if no assumption is to be made (the resulting resource will be the argument), otherwise
  -- Just x should be returned, where x is of type v.
  |> Resource.assumeInCaseNow
      (Resource.therefore Nothing >> Resource.assumeInCaseNow (unknownHtml address >> Just))
  |> Resource.otherwise (errorHtml address)

  |> Resource.

  |> App.presented
  |> App.withChildren [ ]


-- Present the model.
present : Signal.Address (List Action) -> Time -> ModelResource -> App.ViewOutput Action Html ()
present address now model =
  let
    presentListItems

  in
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
update : Action -> Time -> ModelResource -> App.UpdatedModel Action ModelResource ()
update action now model =
  case action of
    NoOp -> App.updated model


-- Set up the program.
output : App.ProgramOutput Action ModelResource Html ()
output =
  App.defProgram present update model0 -- Define your top level program. (defProgram' to use staging.)
  |> App.run     -- Invoke the configured ProgramInput to get a ProgramOutput
  |> App.itself  -- Wire task output back in.


main : Signal Html
main = App.outputView output -- output the view


-- Use sink to run the concrete task output of the program.
port sink : Signal (Task z ())
port sink = App.sink output
