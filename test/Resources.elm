{- Example using Resource for mapping data to views recursively. -}


import Scaffold.App as App
import Scaffold.Machine as Machine exposing (Machine)
import Scaffold.Resource as Res exposing (Resource, ResourceRef, UserTask, ResourceTask)
import Scaffold.Resource.Fire as ResFire


-- Just for show
import Graphics.Element


import Html exposing (Html, div, span, button, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Html.Lazy as HL

import Signal
import Task exposing (Task)

import Dict exposing (Dict)

import Time exposing (Time)

import String


type alias TreeItem =
  { title : String
  , editTitle : Maybe String
  }


-- Our action type.
type Action =
  Delta ModelResource
  | NewItem (List String)
  | EditItem (List String)
  | RevertItem (List String)
  | ChangeItem (List String) String
  | SaveItem (List String) String
  | DeleteItem (List String)


type alias ModelResource =
  Resource () TreeItem


type alias ViewResource =
  Resource () Html


type alias ModelDeltaTask =
  UserTask () TreeItem


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
    , ("top", "5px")
    , ("left", "5px")
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
  let doStyling = styleItem 12 ["arial"] in
    { badStyle = doStyling True True
    , voidStyle = doStyling False True
    , pendingStyle = doStyling True False
    , knownStyle = doStyling False False
    , groupStyle = style [ ("display", "relative"), ("margin-left", "1cm") ]
    }


makeItem : String -> TreeItem
makeItem title =
  { title = title
  , editTitle = Nothing
  }


newItem : TreeItem
newItem =
  { title = ""
  , editTitle = Just ""
  }


editItem : TreeItem -> TreeItem
editItem item =
  changeItem item.title item


changeItem : String -> TreeItem -> TreeItem
changeItem title' item =
  { item | editTitle = Just title' }


saveItem : TreeItem -> TreeItem
saveItem item =
  case item.editTitle of
    Just title' -> { item | editTitle = Nothing, title = title' }
    _ -> item


revertItem : TreeItem -> TreeItem
revertItem item =
  case item.editTitle of
    Just title' -> { item | editTitle = Nothing }
    _ -> item


resources0 : ModelResource
resources0 =
  Res.groupResource
    [ ("personal", Res.groupResource [ ("family", Res.groupResource []), ("friends", Res.groupResource []) ])
    , ("work", Res.groupResource [ ("meetings", Res.groupResource []), ("tasks", Res.groupResource []) ])
    ]
  {-
  Res.groupResource
    [ ("foo", Res.defResource (makeItem "foo value"))
    , ("bar", Res.defResource (makeItem "bar value"))
    , ("fuq", Res.defResource (makeItem "fuq value"))
    ]
  -}


-- Initial model.
model0 : Model
model0 =
    { resources = Res.pendingResource
    , views = Res.pendingResource
    , output = Html.h1 [] [Html.text "Please wait..."]
    , renderContext = defaultRenderContext
    }


-- TODO: add controls!
renderItem : Html.Attribute -> Signal.Address (List Action) -> List String -> TreeItem -> Html
renderItem styleAttrib address rpath item =
  case item.editTitle of
    Nothing ->
      Html.div
        [ styleAttrib ] -- attribs
        [ Html.text ("Item \"" ++ item.title ++ "\".")
        , Html.button [ Html.Events.onClick address [ EditItem rpath ] ] [ Html.text "Edit" ]
        , Html.button [ Html.Events.onClick address [ DeleteItem rpath ] ] [ Html.text "Delete" ]
        ] -- html items
    Just title' ->
      Html.div
        [ styleAttrib ]
        [ Html.input
            [ Html.Attributes.placeholder "Enter Item Title"
            , Html.Attributes.value title'
            , Html.Events.on "input" Html.Events.targetValue (ChangeItem rpath >> flip (::) [ ] >> Signal.message address)
            ]
            [ ]
        , Html.button [ Html.Events.onClick address [ RevertItem rpath ] ] [ Html.text "Revert" ]
        , Html.button [ Html.Events.onClick address [ SaveItem rpath title' ] ] [ Html.text "Save" ]
        ]


-- TODO: improve error output!
renderBad : Html.Attribute -> Signal.Address (List Action) -> ViewResource -> Html
renderBad styleAttrib address res =
  Html.div
    [ styleAttrib ]
    [ Html.text "Unexpected Resource Tag!!!" ]


-- argument given to `Res.flattenDict` for reducing a `ViewResource` in to a single Html element.
-- see below in render for the actual usage.
renderGroup : RenderContext -> Signal.Address (List Action) -> List String -> Dict String ViewResource -> Html
renderGroup renderContext address path resources =
  let
    htmlChildren = Dict.foldr (\k r' -> (::) (render renderContext address (path ++ [k]) r')) []

    htmlGroup children =
      [ Html.button
          [ Html.Events.onClick address [ NewItem path ] ]
          [ Html.text "New Item" ]
      ] ++ children

      |> Html.div [ renderContext.groupStyle ]

    htmlContainer grouped =
      Html.div
        [ style [ ("display", "block") ] ]
        [ Html.h4
            [ styleItem 16 ["arial"] False True ]
            [ Html.abbr
                [ Html.Attributes.title (pathText path) ]
                [ List.head (List.reverse path)
                  |> Maybe.map (flip (++) "/")
                  |> Maybe.withDefault "/"
                  |> Html.text
                ]
            ]
        , grouped
        ]
  in
    resources |> htmlChildren >> htmlGroup >> htmlContainer


renderPlaceholder style' text' =
  Html.div [ style' ] [ Html.text text' ]


-- render a ViewResource recursively. If the given ViewResource is a group, it is reduced to
-- defined HTML, and if void or pending, it is rendered using special placeholders (currently not
-- a lot of effort in to what they look like at all). `otherwise` is used with the special renderBad
-- function to reduce the final output to plain HTML.
render : RenderContext -> Signal.Address (List Action) -> List String -> ViewResource -> Html
render renderContext address rpath res =
  -- recursive flatten function.
  Res.flattenDict (renderGroup renderContext address rpath >> Res.defResource) res

  -- map placeholders and close off with `otherwise`
  |> Res.assumeIfNow Res.isVoid (renderPlaceholder renderContext.voidStyle "Nothing here.")
  |> Res.assumeIfNow Res.isPending (renderPlaceholder renderContext.voidStyle "Please wait.")
  |> Res.otherwise (renderBad renderContext.badStyle address res)


-- modelView is applied to the deltas of the ModelResource to get appropriate deltas for the
-- ViewRes.
modelView : RenderContext -> Signal.Address (List Action) -> List String -> ModelResource -> ViewResource
modelView renderContext address rpath res =
  Res.therefore' (renderItem renderContext.knownStyle address) (List.reverse rpath) res


-- Present the current view output.
present : Signal.Address (List Action) -> Time -> Model -> App.ViewOutput Action Html ()
present address now model =
  App.presented model.output


-- TODO : Add dispatch phase
stage : Signal.Address (List Action) -> Time -> Model -> App.UpdatedModel Action Model ()
stage address now model =
  let
    -- get and transform deltas
    dres = Res.deltaOf model.resources
    dviews = modelView model.renderContext address [ ] dres

    -- update the resource group structures with the deltas.
    resources' = Res.integrate model.resources
    views' = Res.update dviews model.views

    -- render the current view.
    output' = render model.renderContext address [ ] views'
  in
    App.updated
      { model
      | resources = resources'
      , views = views'
      , output = output'
      }


-- Update the model. Nothing unfamiliar here.
update : Action -> Time -> Model -> App.UpdatedModel Action Model ()
update action now model =
  case action of
    Delta dres ->
      App.updated { model | resources = Res.update dres model.resources }

    NewItem path ->
      { model
      | resources =
        Res.writePath (path ++ [""] |> Debug.log "new item path") (Res.defResource newItem) model.resources
      } |> App.updated

    EditItem path ->
      { model
      | resources =
        Res.atPath (Res.therefore editItem) (path |> Debug.log "edit item path") model.resources
      } |> App.updated

    RevertItem path ->
      { model
      | resources =
        Res.atPath (Res.therefore revertItem) (path |> Debug.log "revert item path") model.resources
      } |> App.updated

    ChangeItem path title' ->
      { model
      | resources =
        Res.atPath (Res.therefore <| changeItem title') (path |> Debug.log "change item path") model.resources
      } |> App.updated

    SaveItem path'' title' ->
      List.reverse path''
      |> \rpath'' -> (case rpath'' of
        _ :: rtail -> List.reverse (title' :: rtail)
        [] -> [])
      |> \path' ->
        { model
        | resources =
            Res.getPath (path'' |> Debug.log "save item prior path") model.resources
            |> Res.therefore (changeItem title' >> saveItem)
            |> \saved -> Res.writePath (path' |> Debug.log "save item new path") saved model.resources
            |> Res.deletePath path''
        } |> App.updated

    DeleteItem path ->
      { model
      | resources =
        Res.deletePath (path |> Debug.log "delete item path") model.resources
      } |> App.updated

    --_ ->
    --  App.updated model


-- NOTE: Scaffold.App programs will execute all of the actions implied by it's initial inputs at startup, so
-- it is not neccessary to compute an initial model with the starting metrics for your view. Internally this is
-- done quite easily by using `Signal.Extra.foldp'` as the main engine of `App.run*` instead of `Signal.foldp`.
-- This step is neccessary because the initial values of signals do not produce any events so they will not be
-- immediately translated in to actions.

-- Set up the program.
output : App.ProgramOutput Action Model Html ()
output =
  -- Assemble the ProgramInput functions
  App.defProgram' present stage update model0

  -- We will fill this out in the next example as more sophisticated
  -- layout begins to take place. We will need to pipe in the client
  -- size.
  |> App.defSequenceInputs []

  -- Dispatch an inital task.
  |> App.runAnd (App.actionTask [ Delta resources0 ])

  -- Pipe the results of outgoing tasks back in.
  |> App.itself


main : Signal Html
main = App.outputView output -- output the view


-- Use sink to run the concrete task output of the program.
port sink : Signal (Task z ())
port sink = App.sink output
