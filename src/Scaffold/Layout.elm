{--

Copyright (c) 2016, William Whitacre
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted
provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions
and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions
and the following disclaimer in the documentation and/or other materials provided with the
distribution.

3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

--}


module Scaffold.Layout

  (Axis, Bounds, Group, Item, Rule,

  adjustedRule, horizontalRule, horizontalRuleBetween, horizontalRuleCentered,
  itemHorizontalRule, itemRule, itemVerticalRule, ruleBetween, ruleCenter, snapToRule,
  towardsRule, towardsRuleRelative, verticalRule, verticalRuleBetween, verticalRuleCentered,

  appendGroup, group, groupAt, groupAtBefore, groupFromArray, groupFromDict,
  groupSize, prependGroup, computed, butBefore, butBeforeAt, butBeforeSlice, thenDo, thenDoAt,
  thenDoSlice,

  flatten, flattenOutTo, flattenTo, flattenWithin, flattenWithinOutTo,
  flattenStyled, flattenToStyled, flattenWithinStyled, flattenOutToStyled, flattenWithinOutToStyled,

  spacedBetween, spacedBy, regularly, vertically, horizontally,

  defBounds, autoBounds, boundsHeight, boundsLower, boundsSize, boundsUpper, boundsWidth,
  clampedBounds, clampedInnerBounds, clampedOuterBounds,

  grabItem, grabItemRelative, grabItemRelativeX, grabItemRelativeY, grabItemX, grabItemY,

  itemBounds, itemWidth, itemHeight, itemSize,

  lerpOf, move, moveX, moveY, place, placeX, placeY,

  emptyItem, makeItem, makeItemStyled, iconItem, toItem, fromItem, fromItemStyled)

  where

{-| A very nice ruler snapping symachine for layouts. Actual documentation underway.

# Definitions
@docs Axis, Bounds, Group, Item, Rule

# Ruler Guide Manipulation
@docs adjustedRule, horizontalRule, horizontalRuleBetween, horizontalRuleCentered, itemHorizontalRule, itemRule, itemVerticalRule, ruleBetween, ruleCenter, snapToRule, towardsRule, towardsRuleRelative, verticalRule, verticalRuleBetween, verticalRuleCentered

# Grouping and Group Operations
@docs appendGroup, group, groupAt, groupAtBefore, groupFromArray, groupFromDict, groupSize, prependGroup, computed, butBefore, butBeforeAt, butBeforeSlice, thenDo, thenDoAt, thenDoSlice, flatten, flattenOutTo, flattenTo, flattenWithin, flattenWithinOutTo, flattenStyled, flattenToStyled, flattenWithinStyled, flattenOutToStyled, flattenWithinOutToStyled

# Group Spacing and Distribution.
@docs spacedBetween, spacedBy, regularly, vertically, horizontally

# Bounds Definitions
@docs defBounds, autoBounds, boundsHeight, boundsLower, boundsSize, boundsUpper, boundsWidth, clampedBounds, clampedInnerBounds, clampedOuterBounds

# Item Grabs
@docs grabItem, grabItemRelative, grabItemRelativeX, grabItemRelativeY, grabItemX, grabItemY

# Item Metrics
@docs itemBounds, itemWidth, itemHeight, itemSize

# Item Placement
@docs lerpOf, move, moveX, moveY, place, placeX, placeY

# Create and Display Items
@docs emptyItem, makeItem, makeItemStyled, iconItem, toItem, fromItem, fromItemStyled

-}

import Html exposing (Html)
import Html.Attributes as Attrs

import Svg
import Svg.Attributes as SvgAttrs
import Color

import Json.Encode

import Array exposing (Array)
import Dict exposing (Dict)


type alias Element =
  { html : Html
  , w : Int, h : Int
  }


nodeElement_ : Int -> Int -> Html -> Element
nodeElement_ w h html =
  { html = html
  , w = w, h = h
  }


inPixels_ : Int -> String
inPixels_ = toString >> flip (++) "px"


itemMetricStyle_ : Int -> Int -> Int -> Int -> List (String, String)
itemMetricStyle_ x0 y0 w h =
  [ (,) "position" "absolute"
  , (,) "left"     (inPixels_ x0)
  , (,) "top"      (inPixels_ y0)
  , (,) "width"    (inPixels_ w)
  , (,) "height"   (inPixels_ h)
  ]

itemContainerStyle_ : (Int, Int) -> List (String, String)
itemContainerStyle_ (w, h) =
  [ (,) "position" "relative"
  , (,) "width"  (inPixels_ w)
  , (,) "height" (inPixels_ h)
  ]

emptyHtml_ : Html.Html
emptyHtml_ =
  Html.div [ Attrs.style [ (,) "display" "hidden", (,) "width" "0", (,) "height" "0" ] ] [ ]


itemToHtml_ : List (String, String) -> Item -> Html.Html
itemToHtml_ styles item =
  measure_ item
  |> \{w, h} -> htmlContainer_ w h styles [ item.elem.html ]


htmlContainer_ : Int -> Int -> List (String, String) -> List Html.Html -> Html.Html
htmlContainer_ w h styles htmls =
  Html.div [ (itemContainerStyle_ (w, h)) ++ styles |> Attrs.style ] htmls


{-| An item, which represents an Elm Element with a position and a handle. -}
type alias Item =
  { elem : Element
  , x : Int, u : Int
  , y : Int, v : Int
  }


-- items, pending transformation, sum of widths, sum of heights

{-| Group is an opaque type that represents a grouped set of items with a pending set of transformations. -}
type Group = Group_ (Array Item, Int -> Item -> Item, Int, Int)


-- internal use
type alias ItemMetric_ =
  { x : Int, u : Int, x0 : Int, x1 : Int, w : Int
  , y : Int, v : Int, y0 : Int, y1 : Int, h : Int
  }


{-| A guide ruler defined as either a vertical or a horizontal axis aligned line. -}
type Rule =
  Vertical Int -- x
  | Horizontal Int -- y


{-| An axis along which to evenly distribute by spacing or handle position. -}
type Axis = Vertically | Horizontally

type Arrangement = Spaced | Distributed


{-| A bounding box, given as a minimum and a maximum. -}
type Bounds = BB (Int, Int) (Int, Int) | AutoBB


type alias GroupCons_ = (Array Item, Int, Int)


{-| Automatic bounds. -}
autoBounds : Bounds
autoBounds = AutoBB


{-| Get the minimum bound if a minimum bound is defined. -}
boundsLower : Bounds -> Maybe (Int, Int)
boundsLower bounds =
  case bounds of
    BB (x0, y0) _ -> Just (x0, y0)
    AutoBB -> Nothing


{-| Get the maximum bound if a maximum bound is defined. -}
boundsUpper : Bounds -> Maybe (Int, Int)
boundsUpper bounds =
  case bounds of
    BB _ (x1, y1) -> Just (x1, y1)
    AutoBB -> Nothing


{-| Get the size of a bounding box if both maximum and minimum bounds are defined, otherwise (0, 0). -}
boundsSize : Bounds -> (Int, Int)
boundsSize bounds =
  case (boundsLower bounds, boundsUpper bounds) of
    (Just (x0, y0), Just (x1, y1)) -> (x1 - x0, y1 - y0)
    _ -> (0, 0)


{-| Get the width of a bounding box, defined as boundsSize >> fst -}
boundsWidth : Bounds -> Int
boundsWidth = boundsSize >> fst


{-| Get the width of a bounding box, defined as boundsSize >> snd -}
boundsHeight : Bounds -> Int
boundsHeight = boundsSize >> snd


{-| Define a definite bounding box. -}
defBounds : Int -> Int -> Int -> Int -> Bounds
defBounds x0 y0 x1 y1 =
  BB (x0, y0) (x1, y1)


{-| Clamp a bounding box to some inner bounding box and some outer bounding box. Either the inner
or the outer bounding box may be autoBounds, so this may also only clamp an inner or an outer limit,
or neither. -}
clampedBounds : Bounds -> Bounds -> Bounds -> Bounds
clampedBounds inner outer bb =
  case bb of
    AutoBB ->
      case (inner, outer) of
        (AutoBB, AutoBB) -> bb

        (BB (x0, y0) (x1, y1), _) ->
          defBounds x0 y0 x1 y1

        (AutoBB, BB (x0, y0) (x1, y1)) ->
          defBounds x0 y0 x1 y1

    BB (x0, y0) (x1, y1) ->
      let bb' = BB (x0, y0) (x1, y1) in
        case (inner, outer) of
          (AutoBB, AutoBB) -> bb

          (BB (x0i, y0i) (x1i, y1i), AutoBB) ->
            defBounds (min x0 x0i) (min y0 y0i) (max x1 x1i) (max y1 y1i)

          (AutoBB, BB (x0o, y0o) (x1o, y1o)) ->
            defBounds (max x0 x0o) (max y0 y0o) (min x1 x1o) (min y1 y1o)

          (BB il iu, BB ol ou) ->
            bb' |> clampedBounds (BB il iu) AutoBB >> clampedBounds AutoBB (BB ol ou)


{-| clampedOuterBounds myBounds = clampedBounds autoBounds myBounds -}
clampedOuterBounds : Bounds -> Bounds -> Bounds
clampedOuterBounds = clampedBounds AutoBB


{-| clampedInnerBounds myBounds = clampedBounds myBounds autoBounds -}
clampedInnerBounds : Bounds -> Bounds -> Bounds
clampedInnerBounds = flip clampedBounds AutoBB


{-| An empty layout item. -}
emptyItem : Item
emptyItem = toItem 0 0 emptyHtml_


{-| Create an item from Html -}
toItem : Int -> Int -> Html -> Item
toItem w h htm =
  { elem = nodeElement_ w h htm, x = 0, u = 0, y = 0, v = 0 }


{-| Create an item using an SVG icon function. This is intended primarily for use with the material
icon set, but if others follow suit with the same API for different icon sets, this will work just
as well. Note that the size and color arguments are intentionally flipped. This is for currying
purposes; it is much less likely for the size of an icon to change than the color. -}
iconItem : (Color.Color -> Int -> Svg.Svg) -> Int -> Color.Color -> Item
iconItem fsvg size' color' =
  Svg.svg
    [ SvgAttrs.width (toString size')
    , SvgAttrs.height (toString size')
    , SvgAttrs.viewBox ("0 0 " ++ (toString size') ++ " " ++ (toString size'))
    ]
    [ fsvg color' size'
    ]
  |> toItem size' size'


{-| Create an item using an Html constructor. Nice for when you largely rely on Layout over Html,
using Html just for the end controls. -}
makeItem : Int -> Int -> (List Html.Attribute -> List Html.Html -> Html.Html) -> List Html.Attribute -> List Html.Html -> Item
makeItem w h ctor attrs htmls =
  toItem w h (ctor attrs htmls)


{-| Create an item using an Html constructor, but append a style attribute to the end of the
attributes list with the given styles. Useful in the case that you want to forward declare the
style of a whole bunch of items' Html contents with a partial function.  -}
makeItemStyled : List (String, String) -> Int -> Int -> (List Html.Attribute -> List Html.Html -> Html.Html) -> List Html.Attribute -> List Html.Html -> Item
makeItemStyled styles w h ctor attrs =
  makeItem w h ctor (attrs ++ [ Attrs.style styles ])


{-| Convert an Item to Html by placing it inside of a sized container. -}
fromItem : Item -> Html.Html
fromItem = itemToHtml_ []


{-| The same as fromItem, but styles the sized container with a given arbitrary list of CSS
properties. -}
fromItemStyled : List (String, String) -> Item -> Html.Html
fromItemStyled = itemToHtml_


{-| Get the size of an Item. -}
itemSize : Item -> (Int, Int)
itemSize item =
  (itemWidth item, itemHeight item)


{-| Get the resultant bounding box of an item, given it's placement and handle. -}
itemBounds : Item -> Bounds
itemBounds item =
  let
    {x0, y0, x1, y1} = measure_ item
  in
    defBounds x0 y0 x1 y1

{-| Get the width of an item. -}
itemWidth : Item -> Int
itemWidth =
  measure_ >> .w


{-| Get the height of an item. -}
itemHeight : Item -> Int
itemHeight =
  measure_ >> .h


{-| Grab an item, producing a handle. This defines a point on the item called the "handle" that is
placed exactly at item's coordinates. If I say

  grabItem 0 0

then that would grab the top left corner. u and v and given as window coordinates relative to the
top left corner of the item. -}
grabItem : Int -> Int -> Item -> Item
grabItem u v = grabItemX u >> grabItemY v


{-| Grab an item by it's x coordinate. -}
grabItemX : Int -> Item -> Item
grabItemX u item = { item | u = u }


{-| Grab an item by it's y coordinate. -}
grabItemY : Int -> Item -> Item
grabItemY v item = { item | v = v }


{-| Grab an item in normalized coordinates relative to it's size. This means that (1, 1) is the
bottom right corner, and (0, 0) is the top left corner. -}
grabItemRelative : Float -> Float -> Item -> Item
grabItemRelative u' v' =
  grabItemRelativeX u' >> grabItemRelativeY v'


{-| Grab an item by it's x coordinate relative to the item's size. -}
grabItemRelativeX : Float -> Item -> Item
grabItemRelativeX u' item =
  let {w} = measure_ item
  in grabItemX (floor << (*) u' << toFloat <| w) item


{-| Grab an item by it's y coordinate relative to the item's size. -}
grabItemRelativeY : Float -> Item -> Item
grabItemRelativeY v' item =
  let {h} = measure_ item
  in grabItemY (floor << (*) v' << toFloat <| h) item


{-| Place the handle of an item at these coordinates. -}
place : Int -> Int -> Item -> Item
place x' y' =
  placeX x' >> placeY y'


{-| Place the handle of an item at this x coordinate. -}
placeX : Int -> Item -> Item
placeX x' item =
  { item | x = x' }


{-| Place the handle of an item at this y coordinate. -}
placeY : Int -> Item -> Item
placeY y' item =
  { item | y = y' }


{-| Move an item. -}
move : Int -> Int -> Item -> Item
move dx dy =
  moveX dx >> moveY dy


{-| Move an item vertically. -}
moveY : Int -> Item -> Item
moveY dy item =
  { item | y = item.y + dy }


{-| Move an item horizontally. -}
moveX : Int -> Item -> Item
moveX dx item =
  { item | x = item.x + dx }


{-| Modify any of the item manipulation primitives by linearly interpolating the handle and coordinates
between the current and output values. This provides the basic building block for transitional animations.
Easing functions could quite easily be implemented on top of this. -}
lerpOf : (Item -> Item) -> Float -> Item -> Item
lerpOf f t item =
  let
    {elem, x, y, u, v} = f item

    xi = toFloat item.x
    yi = toFloat item.y
    ui = toFloat item.u
    vi = toFloat item.v
  in
    { elem = elem
    , x = floor (xi - xi * t + (toFloat x) * t)
    , y = floor (yi - yi * t + (toFloat y) * t)
    , u = floor (ui - ui * t + (toFloat u) * t)
    , v = floor (vi - vi * t + (toFloat v) * t)
    }


{-| Create a new vertical ruler at the given x coordinate. -}
verticalRule : Int -> Rule
verticalRule x' = Vertical x'


{-| Create a new horizontal ruler at the given y coordinate. -}
horizontalRule : Int -> Rule
horizontalRule y' = Horizontal y'


{-| Given a ruler direction (verticalRule or horizontalRule), a lower and an upper bound, and some
float t between 0 and 1, linearly interpolate between the lower and upper bound by t. -}
ruleBetween : (Int -> Rule) -> Int -> Int -> Float -> Rule
ruleBetween which l u t =
  toFloat l
  |> \l' -> toFloat u
  |> \u' -> floor (t * u' + l' - (t * l'))
  |> which


{-| Given a ruler direction (verticalRule or horizontalRule), a lower bound, and an upper bound,
center a ruler between the lower and the upper bound. The following equivalencies hold:

    ruleCenter verticalRule x0 x1 == ruleBetween verticalRule x0 x1 0.5
    ruleCenter horizontalRule y0 y1 == ruleBetween horizontalRule y0 y1 0.5

-}
ruleCenter : (Int -> Rule) -> Int -> Int -> Rule
ruleCenter which l u = ruleBetween which l u 0.5


{-| Shorthand for ruleBetween verticalRule -}
verticalRuleBetween : Int -> Int -> Float -> Rule
verticalRuleBetween x x' t = ruleBetween Vertical x x' t


{-| Shorthand for ruleCenter verticalRule -}
verticalRuleCentered : Int -> Int -> Rule
verticalRuleCentered x x' = ruleCenter Vertical x x'


{-| Shorthand for ruleBetween horizontalRule -}
horizontalRuleBetween : Int -> Int -> Float -> Rule
horizontalRuleBetween x x' t = ruleBetween Horizontal x x' t


{-| Shorthand for ruleCenter horizontalRule -}
horizontalRuleCentered : Int -> Int -> Rule
horizontalRuleCentered x x' = ruleCenter Horizontal x x'


blindlyAdjustedRule : Int -> Rule -> Rule
blindlyAdjustedRule ds rule =
  case rule of
    Vertical x' -> Vertical (x' + ds)
    Horizontal y' -> Horizontal (y' + ds)


{-| Adjust a horizontal or vertical ruler. -}
adjustedRule : (Int -> Rule) -> Int -> Rule -> Rule
adjustedRule which ds rule =
  (case rule of
    Horizontal _ -> which == Horizontal
    Vertical _ -> which == Vertical)
  |> \bmatch ->
    if bmatch then
      blindlyAdjustedRule ds rule
    else
      rule


{-| Create a vertical or a horizontal ruler aligned with an item. -}
itemRule : (Int -> Rule) -> Item -> Rule
itemRule which =
  (case which 0 of
    Vertical _ -> .x
    Horizontal _ -> .y
  ) >> which


{-| Create a vertical ruler aligned with an item. -}
itemVerticalRule : Item -> Rule
itemVerticalRule item = itemRule Vertical item


{-| Create a horizontal ruler aligned with an item. -}
itemHorizontalRule : Item -> Rule
itemHorizontalRule item = itemRule Horizontal item


{-| Snap an item's handle to a vertical or horizontal ruler. -}
snapToRule : Rule -> Item -> Item
snapToRule rule item =
  case rule of
    Vertical x' -> { item | x = x' }
    Horizontal y' -> { item | y = y' }


{-| Move an item's handle toward a vertical or horizontal ruler by some constant amount. -}
towardsRule : Rule -> Int -> Item -> Item
towardsRule rule c item =
  case rule of
    Vertical x' -> { item | x = floor (clerp_ (toFloat item.x) (toFloat x') (toFloat c)) }
    Horizontal y' -> { item | y = floor (clerp_ (toFloat item.y) (toFloat y') (toFloat c)) }


{-| Move an item's handle toward a vertical or horizontal ruler by some factor t  -}
towardsRuleRelative : Rule -> Float -> Item -> Item
towardsRuleRelative rule t item =
  case rule of
    Vertical x' -> { item | x = floor (lerp_ (toFloat item.x) (toFloat x') t) }
    Horizontal y' -> { item | y = floor (lerp_ (toFloat item.y) (toFloat y') t) }


groupByFold ffold items =
  let
    getWidth = measure_ >> .w
    getHeight = measure_ >> .h

    (items', w'', h'') =
      ffold
        (\item (arr, w'', h'') ->
          ( Array.push item arr
          , item |> getWidth >> (+) w''
          , item |> getHeight >> (+) h''
          )
        )
        (Array.empty, floor 0, floor 0)
        items
  in
    Group_ (items', always identity, w'', h'')


{-| Create an item group from a list. -}
group : List Item -> Group
group = groupByFold List.foldl


{-| Create an item group from an array. -}
groupFromArray : Array Item -> Group
groupFromArray = groupByFold Array.foldl


{-| Create an item group from a dictionary. -}
groupFromDict : Dict comparable Item -> Group
groupFromDict =
  let
    dictFoldValues ffold ini dct =
      Dict.foldl (always ffold) ini dct

  in
    groupByFold dictFoldValues


{-| Get the size of the group. -}
groupSize : Group -> Int
groupSize grp = Array.length (grparray_ grp)


{-| Append two groups, resulting a group containing the elements from `grp` first and the
elements of `grp'` second. -}
appendGroup : Group -> Group -> Group
appendGroup grp grp' =
  let
    rg = computed grp
    rg' = computed grp'

    items' = Array.append (grparray_ rg) (grparray_ rg')
    w''accum = (grpw''_ rg) + (grpw''_ rg')
    h''accum = (grph''_ rg) + (grph''_ rg')
  in
    Group_ (items', always identity, w''accum, h''accum)


{-| Prepend a group before another, resulting a group containing the elements from `grp` second and
the elements of `grp'` first. -}
prependGroup : Group -> Group -> Group
prependGroup = flip appendGroup


{-| Do a distribution vertically. -}
vertically : Axis
vertically = Vertically


{-| Do a distribution horizontally. -}
horizontally : Axis
horizontally = Horizontally


{-| Distribute the items in the group along some Axis given a starting coordinate and spacing given
in window coordinates, resulting in an evenly spaced horizontal or vertical distribution. Vertical
distribution is done top to bottom, horizontal distribution is done left to right.

The height or width of this vertical or horizontal distribution is

    from + siz * (n - 1) + (sum itemHeight items)

and the coordinate not effected by the distribution is left in place.

-}
spacedBy : Axis -> Int -> Int -> Group -> Group
spacedBy axis from siz grp =
  let
    filledSize =
      case axis of
        Vertically -> grph''_ grp
        Horizontally -> grpw''_ grp

    spaceSize = ((Array.length (grparray_ grp)) - 1) * siz
    totalSize = spaceSize + filledSize
  in
    arrange' Spaced axis from (from + totalSize) grp


{-| Distribute the items in the group along some Axis given a starting coordinate and an ending
coordinate, such that the spaces between all of the items are even, resulting in a justified layout.
Vertical distribution is done top to bottom, horizontal distribution is done left to right.

The height or width of this vertical or horizontal distribution is

    to - from

and the coordinate not effected by the distribution is left in place.
-}
spacedBetween : Axis -> Int -> Int -> Group -> Group
spacedBetween = arrange' Spaced


{-| Distribute the items in the group along some Axis given a starting coordinate and an ending
coordinate, such that the handles of the items are evenly distributed between the starting and the
ending coordinates. The height or width of this vertical or horizontal distribution is dependent on
the positioning of the handles on the items, and the coordinate not effected by the distribution is
left in place. -}
regularly : Axis -> Int -> Int -> Group -> Group
regularly = arrange' Distributed


-- arrange a group either vertically or horizontally using one of two arrangement methods
arrange' : Arrangement -> Axis -> Int -> Int -> Group -> Group
arrange' dist axis l u grp' =
  let
    grp = computed grp'
    array = grparray_ grp

    n = Array.length array
    ncoef = safe_invcoef_ (toFloat n)

    l' = toFloat l
    u' = toFloat u

    distrib f' =
      let
        s0 = l'
        ds = (u' - l') * ncoef

      in
        Array.indexedMap (\i -> f' (floor <| ds * (toFloat i))) array


    verticalArrangement = distrib placeY
    horizontalArrangement = distrib placeX


    spacers f' fs'' fi'' =
      let
        accumSpan = toFloat <| fs'' grp

        nspacers = n - 1
        nspacers_coef = safe_invcoef_ (toFloat nspacers)

        span = u' - l'
        availableSpace = max 0 (span - accumSpan)

        spacerSize = nspacers_coef * availableSpace

        positionSpaced item (lastPos, a') =
          let
            extent = fi'' item
            nextPos = extent + spacerSize + lastPos
          in
            (nextPos, Array.push (f' (floor lastPos) item) a')

        (_, array') = Array.foldl positionSpaced (l', Array.empty) array
      in
        array'


    verticalSpacing = spacers placeY grph''_ (measure_ >> .h >> toFloat)
    horizontalSpacing = spacers placeX grpw''_ (measure_ >> .w >> toFloat)


    array'' =
      case (axis, dist) of
        (Vertically, Distributed) -> verticalArrangement
        (Vertically, Spaced) -> verticalSpacing
        (Horizontally, Distributed) -> horizontalArrangement
        (Horizontally, Spaced) -> horizontalSpacing

  in
    Group_ (array'', always identity, grpw''_ grp, grph''_ grp)


sliceIndex_ len i = if i < 0 then len - i else i

groupAllTransducer_ f _ x = f x

groupWhichTransducer_ len i' f i x =
  sliceIndex_ len i'
  |> \i's -> sliceIndex_ len i
  |> \is_ -> if is_ == i's then f x else x

groupSliceTransducer_ len i' j' f i x =
  sliceIndex_ len i'
  |> \i's -> sliceIndex_ len i'
  |> \j's -> sliceIndex_ len i
  |> \is_ -> if is_ >= i's && is_ < j's then f x else x


groupAppendTransducer_ : (Int -> Item -> Item) -> Group -> Group
groupAppendTransducer_ xdcr grp =
  Group_
    ( grparray_ grp
    , (\i -> grpxdcr_ grp i >> xdcr i)
    , grpw''_ grp
    , grph''_ grp
    )


groupPrependTransducer_ : (Int -> Item -> Item) -> Group -> Group
groupPrependTransducer_ xdcr grp =
  Group_
    ( grparray_ grp
    , (\i -> xdcr i >> grpxdcr_ grp i)
    , grpw''_ grp
    , grph''_ grp
    )

{-| Retrieve the item at a given index in the group, or Nothing if the index is out of bounds. If
there are waiting transformations, a copy of the item with those transformations applied will be
given. -}
groupAt : Int -> Group -> Maybe Item
groupAt i grp =
  groupAtBefore i grp
  |> Maybe.map (grpxdcr_ grp <| i)


{-| Retrieve the item at a given index in the group, or Nothing if the index is out of bounds. The
item will be retrieved in it's original state without any pending group transformations applied. -}
groupAtBefore : Int -> Group -> Maybe Item
groupAtBefore i grp =
  Array.get i (grparray_ grp)


{-| Apply some transformation to every item in the group after the pending transformations. -}
thenDo : (Item -> Item) -> Group -> Group
thenDo f grp =
  groupAppendTransducer_ (groupAllTransducer_ f) grp


{-| Apply some transformation to the item at a particular index in the group after the pending transformations. -}
thenDoAt : Int -> (Item -> Item) -> Group -> Group
thenDoAt i f grp =
  groupAppendTransducer_ (groupWhichTransducer_ (groupSize grp) i f) grp


{-| Apply some transformation to the items in a particular slice of the group after the pending transformations. -}
thenDoSlice : Int -> Int -> (Item -> Item) -> Group -> Group
thenDoSlice i j f grp =
  groupAppendTransducer_ (groupSliceTransducer_ (groupSize grp) i j f) grp


{-| Apply some transformation to every item in the group before the pending transformations. -}
butBefore : (Item -> Item) -> Group -> Group
butBefore f grp =
  groupPrependTransducer_ (groupAllTransducer_ f) grp


{-| Apply some transformation to the item at a particular index in the group before the pending transformations. -}
butBeforeAt : Int -> (Item -> Item) -> Group -> Group
butBeforeAt i f grp =
  groupPrependTransducer_ (groupWhichTransducer_ (groupSize grp) i f) grp


{-| Apply some transformation to the items in a particular slice of the group before the pending transformations. -}
butBeforeSlice : Int -> Int -> (Item -> Item) -> Group -> Group
butBeforeSlice i j f grp =
  groupPrependTransducer_ (groupSliceTransducer_ (groupSize grp) i j f) grp


{-| Run any pending transformations on all group items. For the most part, this can be avoided, but it should be
done if you are planning to get a lot of items back from the group transformed, or else the work of applying the
pending group transforms will be done at least twice, once when you retrieve the item using groupAt and once when
you flatten the group to produce an item. -}
computed : Group -> Group
computed grp =
  Group_ (Array.indexedMap (grpxdcr_ grp) (grparray_ grp), always identity, grpw''_ grp, grph''_ grp)


produce' : Bounds -> Bounds -> List (String, String) -> Group -> (Group, Item)
produce' inner outer styles grp =
  let
    grp' = computed grp


    positionFold item (ls, bb) =
      let {x0, y0, x1, y1} = measure_ item in
        (makePositioned item :: ls, clampedInnerBounds (defBounds x0 y0 x1 y1) bb)


    makePositioned item bound =
      let {x0, y0, w, h} = measure_ item in
        case (boundsLower bound, boundsSize bound) of
          (Just (x0', y0'), (w', h')) ->
            Html.div
              [ itemMetricStyle_ (x0 - x0') (y0 - y0') w h |> Attrs.style ]
              [ item.elem.html ]

          _ -> emptyHtml_


    (bb', item') =
      Array.foldr positionFold ([], inner) (grparray_ grp') -- invert - inward
      |> \(ls', bb_) -> clampedOuterBounds outer bb_
      |> \bb2 -> boundsSize bb2
      |> \(w', h') -> List.map (\f -> f bb2) ls'
      |> htmlContainer_ w' h' styles
      |> toItem w' h'
      |> \item'' -> (bb2, item'')


    (x0', y0') = Maybe.withDefault (0, 0) (boundsLower bb')
  in
    (grp', item' |> grabItem -x0' -y0')


{-| Flatten a group using automatic bounds for the inner and outer bounds. This will produce the
tightest bounding box possible around the target, and place the handle at (0, 0) relative to the resulting
bounding box. This can result in the handle being placed outside of the group, but this can be desirable for
positioning. For example, I may want to grab something at (-5, -5), so I can position it's top left
corner relative to some outer box with (5, 5) padding. -}
flatten : Group -> Item
flatten = flattenStyled []


{-| Flatten a group using an exact bound. If this is specified to be autoBounds, then the following
equivalency holds:

    flattenTo autoBounds group == flatten group

Otherwise, the items in the group are positioned in the resulting item relative to the top left corner of the bounds and
the resulting item shall have the exact size of the given bounds. -}
flattenTo : Bounds -> Group -> Item
flattenTo = flattenToStyled []


{-| Flatten a group using only an outer bound, which is equivalent to

    flattenWithinOutTo autoBounds outer
-}
flattenWithin : Bounds -> Group -> Item
flattenWithin = flattenWithinStyled []


{-| Flatten a group using only an inner bound, which is equivalent to

    flattenWithinOutTo outer autoBounds
-}
flattenOutTo : Bounds -> Group -> Item
flattenOutTo = flattenOutToStyled []


{-| Flatten a group using an inner and an outer bound to clamp the size of the resulting item. -}
flattenWithinOutTo : Bounds -> Bounds -> Group -> Item
flattenWithinOutTo = flattenWithinOutToStyled []


{-| Same as flatten, but applies the given additional styles to the container. -}
flattenStyled : List (String, String) -> Group -> Item
flattenStyled styles = produce' AutoBB AutoBB styles >> snd


{-| Same as flattenTo, but applies the given additional styles to the container. -}
flattenToStyled : List (String, String) -> Bounds -> Group -> Item
flattenToStyled styles exact = flattenWithinOutToStyled styles exact exact


{-| Same as flattenWithin, but applies the given additional styles to the container. -}
flattenWithinStyled : List (String, String) -> Bounds -> Group -> Item
flattenWithinStyled styles = flattenWithinOutToStyled styles AutoBB


{-| Same as flattenOutTo, but applies the given additional styles to the container. -}
flattenOutToStyled : List (String, String) -> Bounds -> Group -> Item
flattenOutToStyled styles = flip (flattenWithinOutToStyled styles) AutoBB


{-| Same as flattenWithinOutTo, but applies the given additional styles to the container. -}
flattenWithinOutToStyled : List (String, String) -> Bounds -> Bounds -> Group -> Item
flattenWithinOutToStyled styles inner outer = produce' inner outer styles >> snd


-- here are a bunch of ugly utility functions that need cleaning someday.
-- for now, they all work.

safe_invcoef_ : Float -> Float
safe_invcoef_ n =
  if n == 0 then 0 else 1 / n


ungrp_ : Group -> (Array Item, Int -> Item -> Item, Int, Int)
ungrp_ grp = case grp of Group_ g_ -> g_

grparray_ : Group -> Array Item
grparray_ g = case g of Group_ (lls, xdcr, w'', h'') -> lls

grpxdcr_ : Group -> Int -> Item -> Item
grpxdcr_ g = case g of Group_ (lls, xdcr, w'', h'') -> xdcr

grpw''_ : Group -> Int
grpw''_ g = case g of Group_ (lls, xdcr, w'', h'') -> w''

grph''_ : Group -> Int
grph''_ g = case g of Group_ (lls, xdcr, w'', h'') -> h''

clamp_ : comparable -> comparable -> comparable -> comparable
clamp_ lower upper = min upper >> max lower

-- measure an item
measure_ : Item -> ItemMetric_
measure_ {elem, x, y, u, v} =
  elem |> \{w, h} ->
    { w = w, h = h
    , x = x, y = y
    , u = u, v = v
    , x0 = x - u
    , y0 = y - v
    , x1 = x - u + w
    , y1 = y - v + h
    }

lerp_ : Float -> Float -> Float -> Float
lerp_ x xp t = (t * xp) + (x - t * x)

clerp_ : Float -> Float -> Float -> Float
clerp_ x xp c =
  if c < 0 then
    clerp_ x xp -c
  else if x == xp then
    x
  else if x < xp then
    min (x + c) xp
  else
    max (x - c) xp
