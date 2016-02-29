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


module Scaffold.Resource

  (Resource,
  ResourceTask, UserTask,
  ResourceRef,
  ResourcePath,

  userTask,

  defResource, forbiddenResource, pendingResource, undecidedResource,
  unknownResource, voidResource, operationResource,

  maybeOr, resultOr,

  assumeIf, assumeIfNot, assumeIfNow, assumeInCase, assumeInCaseNow,
  decideBy, maybeKnownNow,

  therefore, within, otherwise,

  dispatchIf, dispatchIfNot, dispatchInCase, dispatchInCaseNow,

  isUnknown, isNotUnknown,
  isPending, isNotPending,
  isUndecided, isNotUndecided,
  isForbidden, isNotForbidden,
  isVoid, isNotVoid,
  isNil, isNotNil,
  isKnown, isNotKnown,
  isOperation, isNotOperation,

  atPath,
  getPath,

  dispatch,
  integrate,

  toProgram)

  where

{-| Resource system.

# Types
@docs Resource, ResourceTask, ResourceRef, ResourcePath, UserTask

# Functions
@docs userTask, defResource, forbiddenResource, pendingResource, undecidedResource, unknownResource, voidResource, operationResource, maybeOr, resultOr, assumeIf, assumeIfNot, assumeIfNow, assumeInCase, assumeInCaseNow, decideBy, maybeKnownNow, therefore, within, otherwise, dispatchIf, dispatchIfNot, dispatchInCase, dispatchInCaseNow, isUnknown, isNotUnknown, isPending, isNotPending, isUndecided, isNotUndecided, isForbidden, isNotForbidden, isVoid, isNotVoid, isNil, isNotNil, isKnown, isNotKnown, isOperation, isNotOperation, atPath, getPath, dispatch, integrate, toProgram

-}

import Scaffold.App exposing (..)
import Scaffold.Error as Error

import Signal
import Task exposing (Task, andThen, onError)

import Trampoline exposing (Trampoline)

import Dict exposing (Dict)
import Lazy.List exposing (LazyList, (+++))


{-| This is a Task which represents some kind of synchronization with optask data. It can also easily
be used for long running arbitrary computations, too. It produces a Gigan Error or a Resource. -}
type alias ResourceTask euser v = Task (ResourcePath, Error.Error euser) (ResourceRef euser v)


{-| This task simplifies routing out for the user. -}
type alias UserTask euser v = Task (Error.Error euser) (Resource euser v)


{-| A reference to a resource, including it's path. -}
type alias ResourceRef euser v =
  { path : ResourcePath, resource : Resource euser v }


{-| Used as the resource path type. -}
type alias ResourcePath = List String


type alias GroupStruct_ euser v =
  { curr : Dict String (Resource euser v)
  , chgs : Dict String (Resource euser v)
  }


groupStructMap_ f stct =
  let
    curr' = groupChanged_ stct |> .curr

  in
    { curr = Dict.map (always f) curr'
    , chgs = Dict.empty
    }


groupChanged_ stct =
  { curr = Dict.union stct.chgs stct.curr
  , chgs = Dict.empty
  }


groupStructMapChange_ f stct =
  groupChanged_ { stct | chgs = Dict.map (always f) stct.chgs }


groupStructFoldr_ f data stct =
  Dict.foldr
    (\key val data' -> Dict.get key stct.chgs
    |> Maybe.withDefault val
    |> flip (f key) data')
    data
    stct.curr


groupGet_ key stct =
  Maybe.oneOf [Dict.get key stct.chgs, Dict.get key stct.curr]


groupPut_ key value stct =
  { stct | chgs = Dict.insert key value stct.chgs }


groupUpdate_ key op stct =
  groupGet_ key stct
  |> Maybe.map (flip (groupPut_ key) stct)
  |> Maybe.withDefault stct


{-| A resource item. -}
type Resource euser v =
  Unknown
  | Pending
  -- No such datum exists.
  | Void
  -- Gives reason as to why this data are still unknown after a retrieval attempt. If a resource
  -- enters in to this state, it's time to intervene, whether automatically or with the aid of user
  -- feedback of some kind.
  | Undecided (Error.Error euser)
  -- This is what is done if the result from the optask operation for the data is an error explaining why
  -- access to the data was denied.
  | Forbidden (Error.Error euser)
  -- If a resource is an Operation, then any primitives not suffixed with Now will result in an operation
  | Operation (ResourceTask euser v)
  -- known value of type v.
  | Known v
  -- known to be a collection of string keyed v.
  | Group (GroupStruct_ euser v)


{-| True if the resource is unknownResource. -}
isUnknown : Resource euser v -> Bool
isUnknown kb =
  case kb of
    Unknown -> True
    _ -> False


{-| False if the resource is unknownResource. -}
isNotUnknown : Resource euser v -> Bool
isNotUnknown = isUnknown >> not


{-| True if the resource is pendingResource. -}
isPending : Resource euser v -> Bool
isPending kb =
  case kb of
    Pending -> True
    _ -> False

{-| False if the resource is pendingResource. -}
isNotPending : Resource euser v -> Bool
isNotPending = isPending >> not


{-| True if the resource is voidResource. -}
isVoid : Resource euser v -> Bool
isVoid kb =
  case kb of
    Void -> True
    _ -> False


{-| False if the resource is voidResource. -}
isNotVoid : Resource euser v -> Bool
isNotVoid = isVoid >> not


{-| True if the resource is unknownResource or voidResource. -}
isNil : Resource euser v -> Bool
isNil kb =
  case kb of
    Unknown -> True
    Void -> True
    _ -> False


{-| False if the resource is unknownResource or voidResource. -}
isNotNil : Resource euser v -> Bool
isNotNil = isNil >> not


{-| True if the resource is undecidedResource. -}
isUndecided : Resource euser v -> Bool
isUndecided kb =
  case kb of
    Undecided _ -> True
    _ -> False


{-| False if the resource is undecidedResource. -}
isNotUndecided : Resource euser v -> Bool
isNotUndecided = isUndecided >> not


{-| True if the resource is forbiddenResource. -}
isForbidden : Resource euser v -> Bool
isForbidden kb =
  case kb of
    Forbidden _ -> True
    _ -> False


{-| False if the resource is forbiddenResource. -}
isNotForbidden : Resource euser v -> Bool
isNotForbidden = isForbidden >> not


{-| True if the resource is a pending operation. -}
isOperation : Resource euser v -> Bool
isOperation kb =
  case kb of
    Operation _ -> True
    _ -> False


{-| False if the resource is a pending operation. -}
isNotOperation : Resource euser v -> Bool
isNotOperation = isOperation >> not


{-| True if the resource is known. -}
isKnown : Resource euser v -> Bool
isKnown kb =
  case kb of
    Known _ -> True
    _ -> False


{-| False if the resource is known. -}
isNotKnown : Resource euser v -> Bool
isNotKnown = isKnown >> not


{-| True if the resource is unknownResource. -}
isGroup : Resource euser v -> Bool
isGroup kb =
  case kb of
    Group stct -> True
    _ -> False


{-| False if the resource is unknownResource. -}
isNotGroup : Resource euser v -> Bool
isNotGroup = isGroup >> not


{-| Create a task which will route the resulting resource to the given path. -}
userTask : UserTask euser v -> ResourceTask euser v
userTask usertask =
  usertask
    `andThen` (\res -> Task.succeed { resource = res, path = [ ] })
    `onError` (\err' -> Task.fail ([ ], err'))


comprehend : (v -> v') -> ResourceTask euser v -> ResourceTask euser v'
comprehend xdcr optask =
  optask `andThen` (\ref -> { ref | resource = therefore xdcr ref.resource } |> Task.succeed)


routeTo : ResourcePath -> ResourceTask euser v -> ResourceTask euser v
routeTo path' optask =
  optask `andThen` (\ref -> { ref | path = path' } |> Task.succeed)


catchError : (Error.Error euser -> Resource euser v) -> ResourceTask euser v -> ResourceTask euser v
catchError decider optask =
  optask `onError` (\(path', err') -> Task.succeed { path = path', resource = decider err' })


{-| Given a resource of value type v, create a resource of value type v' by transforming the
known value or group using some function (v -> v'). -}
therefore : (v -> v') -> Resource euser v -> Resource euser v'
therefore xdcr kb =
  case kb of
    Unknown -> Unknown
    Pending -> Pending
    Void -> Void

    Undecided err' -> Undecided err'
    Forbidden err' -> Forbidden err'
    Known x' -> Known (xdcr x')
    Group stct -> Group <| groupStructMap_ (therefore xdcr) stct

    Operation optask -> Operation (comprehend xdcr optask)


{-| DEPRECIATED version of therefore, not supporting type transformation. -}
within : (sub -> sub) -> Resource euser sub -> Resource euser sub
within = therefore


{-| Manipulate an item at the given path, or else do nothing if the path does not exist. -}
atPath : (Resource euser v -> Resource euser v) -> List String -> Resource euser v -> Resource euser v
atPath operation path kb =
  case path of
    [ ] -> operation kb
    element :: path' ->
      case kb of
        Group stct ->
          groupUpdate_ element (atPath operation path') stct
          |> Group

        _ -> kb


{-| Get the item at the given path. Returns unknownResource if the item _might_ exist, but the hierarchy
does not show knowledge at the fringe (i.e., the fringe is unknown at the last known location in
the path), but may also return voidResource to a path which is known not to exist. For example,
if foo is a resource, then foo/bar cannot be a valid path because foo is not a collection. Pending
will be given in the case that an operation is pending. -}
getPath : List String -> Resource euser v -> Resource euser v
getPath path kb =
  case path of
    [ ] -> kb
    element :: path' ->
      case kb of
        Group stct ->
          groupGet_ element stct
          |> Maybe.map (getPath path')
          |> Maybe.withDefault Unknown

        Known x' -> Void
        Void -> Void

        Pending -> Pending
        Operation optask -> Pending

        _ -> Unknown


{-| Offer a decision on some `undecidedResource kb`. Undecided resource is the result of some
problem which may or may not be in control of the client. Such resource may be the result of
anything that can result in an error in your application. If this resource is an operation, then
the assumption will be applied to the result of that operation. -}
decideBy : (Error.Error euser -> Resource euser v) -> Resource euser v -> Resource euser v
decideBy decider kb =
  case kb of
    Undecided err' -> decider err'
    Operation optask -> Operation (catchError decider optask)

    _ -> kb


{-| If some predicate `satisfies` is satisfied by the resource `kb`, then we make the following
assumption. If this resource is an operation, then the assumption will be applied to
the result of that operation. -}
assumeIf : (Resource euser v -> Bool) -> v -> Resource euser v -> Resource euser v
assumeIf satisfies assume kb =
  case kb of
    Operation optask ->
      Operation (optask
        `andThen` (\ref -> { ref | resource = assumeIf satisfies assume ref.resource } |> Task.succeed))

    _ ->
      if satisfies kb then therefore (always assume) kb else kb


{-| Negation of assumeIf. -}
assumeIfNot : (Resource euser v -> Bool) -> v -> Resource euser v -> Resource euser v
assumeIfNot satisfies assume kb =
  assumeIf (satisfies >> not) assume kb


{-| If `possibleAssumption` yields some value `value'` when a Resource is applied, then
that value is used to overwrite the resource with an assumption `Known value'`, otherwise the
Resource is unaffected. If this resource is an operation, then the assumption will be applied
conditionally to the result of that operation. -}
assumeInCase : (Resource euser v -> Maybe v) -> Resource euser v -> Resource euser v
assumeInCase possibleAssumption kb =
  case kb of
    Operation optask ->
      Operation (optask
        `andThen` (\ref -> { ref | resource = assumeInCase possibleAssumption ref.resource } |> Task.succeed))

    _ ->
      Maybe.map Known (possibleAssumption kb)
      |> Maybe.withDefault kb


{-| If some predicate `satisfies` is satisfied by the resource `kb`, then we make the following
optask. -}
dispatchIf : (Resource euser v -> Bool) -> ResourceTask euser v -> Resource euser v -> Resource euser v
dispatchIf satisfies optask kb =
  case kb of
    Operation optask ->
      Operation (optask
        `andThen` (\ref -> { ref | resource = dispatchIf satisfies optask ref.resource } |> Task.succeed))

    _ ->
      dispatchInCase (if satisfies kb then always (Just optask) else always Nothing) kb


{-| Negation of dispatchIf -}
dispatchIfNot : (Resource euser v -> Bool) -> ResourceTask euser v -> Resource euser v -> Resource euser v
dispatchIfNot satisfies optask kb =
  dispatchIf (satisfies >> not) optask kb


{-| If `possibleOperation` yields some ResourceTask task `optask` when a Resource is applied, then
the resource is replaced by the resource `operationResource optask`, otherwise the resource is
unaffected. If this resource is an operation, then the result of that operation will be used as
the input to the provided function. In this way, operations can be chained arbitrarily deep,
but in a manner that helpfully abstracts away whether we are still waiting or already have the
result in the composition. -}
dispatchInCase : (Resource euser v -> Maybe (ResourceTask euser v)) -> Resource euser v -> Resource euser v
dispatchInCase possibleOperation kb =
  case kb of
    Operation optask ->
      Operation (optask
        `andThen` (\ref -> { ref | resource = dispatchInCase possibleOperation ref.resource } |> Task.succeed))

    _ ->
      Maybe.map operationResource (possibleOperation kb)
      |> Maybe.withDefault kb


-- NOTE : These primitives force a reduction now even for an optask operation type
-- convenient conversion to a maybe after all mappings are given. This is intended for use when
-- mapping the state of the content to an actual display.

{-| If a resource is known, then give Just it's value, otherwise Nothing. -}
maybeKnownNow : Resource euser v' -> Maybe v'
maybeKnownNow kb' =
  case kb' of
    Known x' -> Just x'
    _ -> Nothing


{-| If the predicate is satisfied, replace the resource with some known value. -}
assumeIfNow : (Resource euser v' -> Bool) -> v' -> Resource euser v' -> Resource euser v'
assumeIfNow satisfies assumption kb' =
  if satisfies kb' then Known assumption else kb'


{-| This is the counterpart to assumeInCase which does _not_ abstract away whether or not this is
some pending optask operation. Concretely, we want this in the case that we are doing model to view
reductions because a pending operation should still have some concrete visible representation, such
as an ajax loader symbol. Of course, one should still correctly call *Integrate so that an operation
is always a `pendingResource` by the time it gets past the `stage` step. -}
assumeInCaseNow : (Resource euser v' -> Maybe v') -> Resource euser v' -> Resource euser v'
assumeInCaseNow possibleAssumption kb' =
  Maybe.map Known (possibleAssumption kb')
  |> Maybe.withDefault kb'


{-|  -}
dispatchInCaseNow : (Resource euser v -> Maybe (ResourceTask euser v)) -> Resource euser v -> Resource euser v
dispatchInCaseNow possibleOperation kb =
  case kb of
    Operation optask ->
      Operation (optask
        `andThen` (\ref -> { ref | resource = dispatchInCase possibleOperation ref.resource } |> Task.succeed))

    _ ->
      Maybe.map operationResource (possibleOperation kb)
      |> Maybe.withDefault kb


{-|  -}
otherwise : v' -> Resource euser v' -> v'
otherwise assumption kb' =
  case kb' of
    Known x' -> x'
    _ -> assumption


{-|  -}
unknownResource : Resource euser v
unknownResource = Unknown


{-|  -}
pendingResource : Resource euser v
pendingResource = Pending


{-|  -}
voidResource : Resource euser v
voidResource = Void


{-|  -}
undecidedResource : Error.Error euser -> Resource euser v
undecidedResource = Undecided


{-|  -}
forbiddenResource : Error.Error euser -> Resource euser v
forbiddenResource = Forbidden


{-|  -}
operationResource : ResourceTask euser v -> Resource euser v
operationResource = Operation


{-|  -}
defResource : v -> Resource euser v
defResource = Known


{-|  -}
resultOr : (Error.Error euser -> Resource euser v) -> Result (Error.Error euser) v -> Resource euser v
resultOr errorResource result =
  case result of
    Result.Ok data -> defResource data
    Result.Err err' -> errorResource err'


{-| -}
maybeOr : Resource euser v -> Maybe v -> Resource euser v
maybeOr nothingResource maybeValue =
  Maybe.map Known maybeValue
  |> Maybe.withDefault nothingResource



{-| Given some configuration and a resource, produce Just an opaque query task or Nothing
when the resource is an operation or the resource is not an operation respectively. -}
dispatch : Resource euser v -> List (ResourceTask euser v)
dispatch kb =
  dispatch_ [] kb
  |> Lazy.List.toList -- crunch to a normal list at top level.


dispatch_ : List String -> Resource euser v -> LazyList (ResourceTask euser v)
dispatch_ rpath kb =
  case kb of
    Group stct -> groupStructFoldr_ (\key kb ls -> (dispatch_ (key :: rpath) kb) +++ ls) Lazy.List.empty stct

    Operation optask ->
      Lazy.List.singleton
        (routeTo (List.reverse rpath) optask
          `onError` (\(path', err') -> Task.succeed { path = path', resource = undecidedResource err' }))

    _ -> Lazy.List.empty


{-| Given some configuration and a resource, produce a pendingResource in the case that the
resource is an operation, otherwise give the same resource. -}
integrate : Resource euser v -> Resource euser v
integrate kb =
  case kb of
    Group stct -> groupStructMapChange_ integrate stct |> Group
    Operation optask -> Pending
    _ -> kb


{-| -}
toProgram : (b -> ProgramInput a b c bad) -> Resource euser b -> Resource euser (ProgramInput a b c bad)
toProgram modelInput model' = therefore modelInput model'
