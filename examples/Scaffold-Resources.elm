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
  Delta ModelResource


type alias ModelResource =
  Resource () TreeItem


type alias ModelDeltaTask =
  UserTask () TreeItem


type alias ViewResource =
  Resource () Html


type alias RenderContext =
  { badStyle : Html.Attribute
  , voidStyle : Html.Attribute
  , pendingStyle : Html.Attribute
  , knownStyle : Html.Attribute
  , groupStyle : Html.Attribute
  }


type alias Model =
  { renderContext : RenderContext
  , resources : ModelResource
  , views : ViewResource
  , output : Html
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
    , ("width", "660px")
    ]


styleItem : Int -> List String -> Bool -> Bool -> Html.Attribute
styleItem fontsize fontfaces itf bf =
  style
    [ ("display", "relative")
    , ( "font"
      , (toString fontsize)
        ++ "px "
        ++ (List.map (\x -> "\"" ++ x ++ "\"") fontfaces |> String.join ", ")
      )
    ]


pathText : List String -> String
pathText = List.foldr (\elem p -> ("/" ++ elem) ++ p) ""


defaultRenderContext : RenderContext
defaultRenderContext =
  let doStyling = styleItem 12 "arial" in
    { badStyle = doStyling True True
    , voidStyle = doStyling False True
    , pendingStyle = doStyling True False
    , knownStyle = doStyling False False
    , groupStyle = style [ ("display", "relative"), ("margin-left", "1cm") ]
    }


resources0 : ModelResource
resources0 =
  Resource.groupResource
    [ ("foo", Resource.defResource "foo value")
    , ("bar", Resource.defResource "bar value")
    , ( "baz"
      , Resource.groupResource
          [ ("child1", "first baz child")
          , ("child2", "second baz child")
          ]
      )
    , ("fuq", Resource.defResource "fuq value")
    ]


-- Initial model.
model0 : Model
model0 =
    { resources = Resource.pendingResource
    , views = Resource.pendingResource
    , output = HL.lazy2 Html.h1 [] [Html.text "Please wait..."]
    , renderContext = defaultRenderContext
    }


-- TODO: add controls!
renderItem : Html.Attribute -> Signal.Address (List Action) -> TreeItem -> Html
renderItem styleAttrib address (key, datum) =
  Html.div
    [ styleAttrib ]                                          -- attribs
    [ Html.text "\"" ++ key ++ "\" => \"" ++ datum ++ "\"" ] -- html items


-- TODO: improve error output!
renderBad : Html.Attribute -> Signal.Address (List Action) -> ModelResource -> Html
renderBad styleAttrib address res =
  Html.div
    [ styleAttrib ]
    [ HL.lazy Html.text ((++) "Unexpected Resource Tag :: " <| toString res) ]


-- argument given to `Resource.flattenDict` for reducing a `ViewResource` in to a single Html element.
-- see below in render for the actual usage.
renderGroup : RenderContext -> Signal.Address (List Action) -> List String -> Dict String ViewResource -> Html
renderGroup renderContext address rpath resources =
  let
    htmlChildList =
      Dict.foldr
        (\k r' htmlList ->
          (HL.lazy3 (uncurry render) (renderContext, address) (k :: rpath) r') :: htmlList)
        []

    htmlChildren (_, _, _, res) =
      htmlChildList res

    htmlGroup =
      Html.div [ renderContext.groupStyle ]

    htmlContainer grouped =
      Html.div
      [ Html.h4
          [ styleItem 16 "arial" False True ]
          [ Html.abbr
              [ Html.Attributes.title (pathText rpath) ]
              [ List.head rpath
                |> Maybe.map (flip (++) "/")
                |> Maybe.withDefault "/"
              ]
          ]
      , grouped
      ]
  in
    HL.lazy
      (htmlChildren >> htmlGroup >> htmlContainer)
      (renderContext, address, rpath, resources)


-- render a ViewResource recursively. If the given ViewResource is a group, it is reduced to
-- defined HTML, and if void or pending, it is rendered using special placeholders (currently not
-- a lot of effort in to what they look like at all). `otherwise` is used with the special renderBad
-- function to reduce the final output to plain HTML.
render : RenderContext -> Signal.Address (List Action) -> List String -> ViewResource -> Html
render renderContext address rpath res = res
  |> Resource.deriveIfNow Resource.isGroup
      (Resource.flattenDict (renderGroup renderContext address rpath) >> Resource.defResource)

  -- replace me with a control.
  >> Resource.assumeIfNow Resource.isVoid
      (HL.lazy3 renderItem renderContext.voidStyle address (key, "Nothing here!"))

  -- replace me with a better indicator
  >> Resource.assumeIfNow Resource.isPending
      (HL.lazy3 renderItem renderContext.pendingStyle address (key, "Please wait..."))

  -- in any other case, simply use the renderBad function.
  >> Resource.otherwise (HL.lazy3 renderBad renderContext.badStyle address res)


-- modelView is applied to the deltas of the ModelResource to get appropriate deltas for the
-- ViewResource.
modelView : RenderContext -> Signal.Address (List Action) -> List String -> ModelResource -> ViewResource
modelView renderContext address rpath res =
  Resource.therefore (HL.lazy3 renderItem renderContext.knownStyle address) res


-- Present the current view output.
present : Signal.Address (List Action) -> Time -> Model -> App.ViewOutput Action Html ()
present address now model =
  App.presented model.output


-- collapse : (comparable -> Resource euser v -> v -> Resource euser v) -> v -> Resource euser v -> Resource euser v

-- Update the model. Nothing unfamiliar here.
stage : Signal.Address (List Action) -> Time -> Model -> App.UpdatedModel Action Model ()
stage address now model =
  let
    -- Transform the changes to the model to a view delta using our modelView function
    dviews =
      Resource.deltaTo (modelView model.renderContext address [ ]) model.resources

    -- Update the view structure with the delta we got.
    views' =
      Resource.update dviews model.views

    -- Render the new view. Html.Lazy ensures that the recursion stops where the view tree structure
    -- has not changed, so we only create new VirtualDOM the _fingers of the changes to the tree_.
    foutput _ v' =
      render model.renderContext address [ ] v'

  in
    { model
    | views = views',
    , output = HL.lazy2 foutput (model.renderContext, address, [ ]) views'
    }

    |> App.presented



-- Update the model. Nothing unfamiliar here.
update : Action -> Time -> ModelResource -> App.UpdatedModel Action ModelResource ()
update action now model =
  case action of
    Delta dres ->
      (Delta >> flip (::) [])

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
