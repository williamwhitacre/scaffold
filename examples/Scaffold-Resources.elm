{- Example using Resource for mapping data to views recursively. -}

import Scaffold.App as App
import Scaffold.Machine as Machine exposing (Machine)
import Scaffold.Resource as Resource exposing (Resource, ResourceRef, UserTask, ResourceTask)


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
  Resource.groupResource []
  {-
  Resource.groupResource
    [ ("foo", Resource.defResource (makeItem "foo value"))
    , ("bar", Resource.defResource (makeItem "bar value"))
    , ("fuq", Resource.defResource (makeItem "fuq value"))
    ]
  -}


-- Initial model.
model0 : Model
model0 =
    { resources = Resource.pendingResource
    , views = Resource.pendingResource
    , output = Html.h1 [] [Html.text "Please wait..."]
    , renderContext = defaultRenderContext
    }


-- TODO: add controls!
renderItem : Html.Attribute -> Signal.Address (List Action) -> List String -> TreeItem -> Html
renderItem styleAttrib address rpath item =
  Debug.log "render item at" (List.reverse rpath)
  |> \_ -> case item.editTitle of
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


-- argument given to `Resource.flattenDict` for reducing a `ViewResource` in to a single Html element.
-- see below in render for the actual usage.
renderGroup : RenderContext -> Signal.Address (List Action) -> List String -> Dict String ViewResource -> Html
renderGroup renderContext address rpath resources =
  let
    htmlChildren =
      Dict.foldr
        (\k r' htmlList -> (render renderContext address (Debug.log "render child" <| k :: rpath) r') :: htmlList)
        []

    htmlGroup children =
      let
        buttons =
          [ Html.button [ Html.Events.onClick address [ NewItem rpath ] ] [ Html.text "New Item" ]
          ]

      in
        Html.div [ renderContext.groupStyle ] (buttons ++ children)

    htmlContainer grouped =
      Html.div
        [ style [ ("display", "block") ] ]
        [ Html.h4
            [ styleItem 16 ["arial"] False True ]
            [ Html.abbr
                [ Html.Attributes.title (pathText rpath) ]
                [ List.head rpath
                  |> Maybe.map (flip (++) "/")
                  |> Maybe.withDefault "/"
                  |> Html.text
                ]
            ]
        , grouped
        ]
  in
    resources |> htmlChildren >> htmlGroup >> htmlContainer


-- render a ViewResource recursively. If the given ViewResource is a group, it is reduced to
-- defined HTML, and if void or pending, it is rendered using special placeholders (currently not
-- a lot of effort in to what they look like at all). `otherwise` is used with the special renderBad
-- function to reduce the final output to plain HTML.
render : RenderContext -> Signal.Address (List Action) -> List String -> ViewResource -> Html
render renderContext address rpath res =
  res

  |> (Resource.flattenDict (renderGroup renderContext address (Debug.log "render at path" rpath) >> Resource.defResource)
      -- replace me with a control.
      >> Resource.assumeIfNow Resource.isVoid
          (Html.div [ renderContext.voidStyle ] [ Html.text "Nothing here!" ])

      -- replace me with a better indicator
      >> Resource.assumeIfNow Resource.isPending
          (Html.div [ renderContext.pendingStyle ] [ Html.text "Please wait!" ])

      -- in any other case, simply use the renderBad function.
      >> Resource.otherwise (renderBad renderContext.badStyle address res)
    )


-- modelView is applied to the deltas of the ModelResource to get appropriate deltas for the
-- ViewResource.
modelView : RenderContext -> Signal.Address (List Action) -> List String -> ModelResource -> ViewResource
modelView renderContext address rpath res =
  Resource.therefore' (renderItem renderContext.knownStyle address) (List.reverse rpath) res


-- Present the current view output.
present : Signal.Address (List Action) -> Time -> Model -> App.ViewOutput Action Html ()
present address now model =
  App.presented model.output


-- collapse : (comparable -> Resource euser v -> v -> Resource euser v) -> v -> Resource euser v -> Resource euser v

-- Update the model. Nothing unfamiliar here.
-- TODO : Add dispatch phase
stage : Signal.Address (List Action) -> Time -> Model -> App.UpdatedModel Action Model ()
stage address now model =
  let
    dres =
      Resource.deltaOf model.resources

    -- Transform the changes to the model to a view delta using our modelView function
    dviews =
      modelView model.renderContext address [ ] dres

    -- Update the view structure with the delta we got.
    views' =
      Resource.update dviews model.views

    -- Resource.dispatch resources'
    -- |> List.map deltaTask
  in
    { model
    | resources = Resource.integrate model.resources
    , views = views'
    , output = render model.renderContext address [ ] views'
    }

    |> App.updated



-- Update the model. Nothing unfamiliar here.
update : Action -> Time -> Model -> App.UpdatedModel Action Model ()
update action now model =
  case action of
    Delta dres ->
      { model
      | resources = Resource.update dres model.resources
      }

      |> App.updated

    NewItem rpath ->
      { model
      | resources = Resource.writePath ("" :: rpath |> List.reverse) (Resource.defResource newItem) model.resources
      }

      |> App.updated


    EditItem rpath ->
      { model
      | resources =
          Resource.atPath (Resource.therefore editItem) (Debug.log "EditItem path is" <| List.reverse rpath) model.resources
      }

      |> App.updated


    RevertItem rpath ->
      { model
      | resources =
          Resource.atPath (Resource.therefore revertItem) (Debug.log "RevertItem path is" <| List.reverse rpath) model.resources
      }

      |> App.updated

    ChangeItem rpath title' ->
      { model
      | resources =
          Resource.atPath (Resource.therefore <| changeItem title') (Debug.log "ChangeItem path is" <| List.reverse rpath) model.resources
      }

      |> App.updated

    SaveItem (rtitle :: rtail) title' ->
      List.reverse (rtitle :: rtail)
      |> \path'' -> List.reverse (title' :: rtail)
      |> \path' ->
        { model
        | resources =
            Resource.atPath (Resource.therefore <| saveItem << changeItem title') (Debug.log "SaveItem path is" path'') model.resources
            |> Resource.movePath Resource.chooseLeft path'' path'
        }

        |> App.updated

    DeleteItem rpath ->
      { model
      | resources =
          Resource.deletePath (Debug.log "DeleteItem path is" <| List.reverse rpath) model.resources
      }

      |> App.updated


-- toProgramTask : (Error.Error euser -> List a) -> (Resource euser v -> List a) -> UserTask euser v -> ProgramTask bad a

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
