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
  Delta (Resource () TreeItem)
  | Dispatch (UserTask () TreeItem)


type alias ModelResource =
  Resource () TreeItem


type alias ViewResource =
  Resource () Html


type alias RenderContext =
  { badStyle : Html.Attribute
  , voidStyle : Html.Attribute
  , pendingStyle : Html.Attribute
  , knownStyle : Html.Attribute
  }


type alias Model =
  { renderContext : RenderContext
  , resources : ModelResource
  , views : ViewResource
  }


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


styleItem : Int -> List String -> Bool -> Bool -> Html.Attribute
styleItem fontsize fontfaces itf bf =
  style
    [ ("display", "inline-block")
    , ( "font"
      , (toString fontsize)
        ++ "px "
        ++ (List.map (\x -> "\"" ++ x ++ "\"") fontfaces |> String.join ", ")
      )
    ]


defaultRenderContext : RenderContext
defaultRenderContext =
  let doStyling = styleItem 12 "arial" in
    { badStyle = doStyling True True
    , voidStyle = doStyling False True
    , pendingStyle = doStyling True False
    , knownStyle = doStyling False False
    }


-- Initial model.
model0 : Model
model0 =
  { resources = Resource.groupResource
      [ ("foo", Resource.defResource "foo value")
      , ("bar", Resource.defResource "bar value")
      , ("baz", Resource.defResource "baz value")
      ]
  , renderContext = defaultRenderContext
  }


-- TODO: add controls!
renderItem : Html.Attribute -> Signal.Address (List Action) -> TreeItem -> Html
renderItem styleAttrib address (key, datum) =
  HL.lazy2 Html.div
    [ styleAttrib ]                              -- attribs
    [ Html.text "\"" ++ key ++ "\": " ++ datum ] -- html items


renderBad : Html.Attribute -> Signal.Address (List Action) -> ModelResource -> Html
renderBad styleAttrib address res =
  HL.lazy2 Html.div [ styleAttrib ] [ HL.lazy Html.text ((++) "Unexpected Resource Tag :: " <| toString res) ]


render : RenderContext -> Signal.Address (List Action) -> ModelResource -> 

renderGroup : RenderContext -> Signal.Address (List Action) -> ModelResource -> Html
renderGroup renderContext address res =
  Resource.flatten


-- remember that ModelResource = Resource () TreeItem
renderLeaf : RenderContext -> Signal.Address (List Action) -> ModelResource -> Html
renderLeaf renderContext address res =
  Resource.therefore (HL.lazy3 renderItem renderContext.knownStyle address) res

  |> Resource.assumeInCaseNow
      (\res' -> if isGroup res' then renderGroup renderContext address res' )

  -- replace me with a control.
  |> Resource.assumeIfNow Resource.isVoid
      (HL.lazy3 renderItem renderContext.voidStyle address (key, "Nothing here!"))

  -- replace me with a better indicator
  |> Resource.assumeIfNow Resource.isPending
      (HL.lazy3 renderItem renderContext.pendingStyle address (key, "Please wait..."))

  -- in any other case, simply use the renderBad function.
  |> Resource.otherwise (HL.lazy3 renderBad renderContext.badStyle address res)


-- Present the model.
present : Signal.Address (List Action) -> Time -> Model -> App.ViewOutput Action Html ()
present address now model =
  renderModelResource model.renderContext address now model.resources

  |> App.presented
  |> App.withChildren [ ]


-- collapse : (comparable -> Resource euser v -> v -> Resource euser v) -> v -> Resource euser v -> Resource euser v

-- Update the model. Nothing unfamiliar here.
stage : Signal.Address (List Action) -> Time -> ModelResource -> App.UpdatedModel Action Model ()
stage address now model =


  let
    foldHtml key res ls =
      renderModelResource renderContext address now

    renderHtml
  in
    Resource.throughout (renderModelResource renderContext address >> Resource.defResource) res
    |> Resource.collapse foldHtmlLists []



-- Update the model. Nothing unfamiliar here.
update : Action -> Time -> ModelResource -> App.UpdatedModel Action ModelResource ()
update action now model =
  case action of
    NoOp -> App.updated model


-- Set up the program.
output : App.ProgramOutput Action ModelResource Html ()
output =
  App.defProgram' present stage update model0 -- Define your top level program.
  |> App.run     -- Invoke the configured ProgramInput to get a ProgramOutput
  |> App.itself  -- Wire task output back in.


main : Signal Html
main = App.outputView output -- output the view


-- Use sink to run the concrete task output of the program.
port sink : Signal (Task z ())
port sink = App.sink output
