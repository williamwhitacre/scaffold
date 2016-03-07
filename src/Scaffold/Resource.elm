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

  userTask, deltaTask, toProgramTask,

  defResource, forbiddenResource, pendingResource, undecidedResource,
  unknownResource, voidResource, operationResource, groupResource,

  maybeOr, resultOr,

  assumeIf, assumeIfNot, assumeIfNow, assumeInCase, assumeInCaseNow,
  dispatchIf, dispatchIfNot, dispatchInCase, dispatchInCaseNow,
  deriveIf, deriveIfNow, decideBy, maybeKnownNow,

  comprehend, interpret, routeTo, catchError,
  collapse, flatten, flattenDict, throughout, throughoutNow, therefore, within, otherwise,


  isUnknown, isNotUnknown,
  isPending, isNotPending,
  isUndecided, isNotUndecided,
  isForbidden, isNotForbidden,
  isVoid, isNotVoid,
  isNil, isNotNil,
  isKnown, isNotKnown,
  isOperation, isNotOperation,
  isGroup, isNotGroup,

  prefixPath,
  atPath,
  putPath,
  getPath,

  chooseLeft,
  chooseRight,
  chooseVoid,
  chooseNeither,

  merge, mergeMany,

  update, updateList,
  update', updateList',
  dispatch, integrate,
  deltaTo, deltaOf,

  toProgram)

  where

{-| Resource system.

# Types
@docs Resource, ResourceTask, ResourceRef, ResourcePath, UserTask

# Define Resources
@docs defResource, forbiddenResource, pendingResource, undecidedResource, unknownResource, voidResource, operationResource, groupResource

# Interpret `Maybe` or `Result` as Resources
@docs maybeOr, resultOr

# Conditional Assumptions
@docs assumeIf, assumeIfNot, assumeIfNow, assumeInCase, assumeInCaseNow

# Conditional Derivations

`deriveIf` and `deriveIfNow` are given as more flexible and readable versions of `assumeInCase`
and `assumeInCaseNow`. Neither muddies the waters with the Maybe type, and the transformation
can be any `Resource euser v -> Resource euser v`, which makes these highly nestable by comparison
to their respective older counterparts.

@docs deriveIf, deriveIfNow

# Resource Output
@docs otherwise, maybeKnownNow

# Bulk Operations
@docs decideBy, flatten, flattenDict, collapse, throughout, throughoutNow, therefore, within

# Handling `UserTask` and `ResourceTask`
@docs userTask, deltaTask, toProgramTask, comprehend, interpret, routeTo, catchError

# Conditional Operations
@docs dispatchIf, dispatchIfNot, dispatchInCase, dispatchInCaseNow

# Resource Introspection Predicates
@docs isUnknown, isNotUnknown, isPending, isNotPending, isUndecided, isNotUndecided, isForbidden, isNotForbidden, isVoid, isNotVoid, isNil, isNotNil, isKnown, isNotKnown, isOperation, isNotOperation, isGroup, isNotGroup

# Manipulate and Use `ResourcePath`
@docs prefixPath, atPath, putPath, getPath

# Built-in Conflict Operators

For use with `merge` and `mergeMany`.

@docs chooseLeft, chooseRight, chooseVoid, chooseNeither

# Merge Resource Groups
@docs merge, mergeMany

# Update Resources
@docs update, updateList, update', updateList', dispatch, integrate, deltaTo, deltaOf

# Program Resources
@docs toProgram

-}

import Scaffold.App exposing (..)
import Scaffold.Error as Error

import Signal
import Task exposing (Task, andThen, onError)

import Trampoline exposing (Trampoline)

import Dict exposing (Dict)
import Set exposing (Set)
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
  { stct | chgs = groupStructMapped_ f stct }


groupStructMapped_ f stct =
  let
    curr' = groupStructChanged_ stct |> .curr

  in
    Dict.map (always f) curr'


groupStructChanged_ stct =
  { curr =
    Dict.foldl
      (\key value -> case value of
        Unknown -> Dict.remove key
        Group stct -> Dict.insert key (groupStructChanged_ stct |> Group)
        _ -> Dict.insert key value)
      stct.curr
      stct.chgs

  , chgs = Dict.empty
  }


groupStructMapChange_ f stct =
  groupStructChanged_ { stct | chgs = Dict.map (always f) stct.chgs }


groupStructFoldHelper_ foldf f data stct =
  foldf f data (groupStructChanged_ stct |> .curr)


groupStructFoldl_ f data stct =
  groupStructFoldHelper_ Dict.foldl f data stct


groupStructFoldr_ f data stct =
  groupStructFoldHelper_ Dict.foldr f data stct


groupGet_ key stct =
  Maybe.oneOf [Dict.get key stct.chgs, Dict.get key stct.curr]
  |> Maybe.withDefault Unknown


groupPut_ key value stct =
  { stct | chgs = Dict.insert key value stct.chgs }


groupUpdate_ key op stct =
  groupGet_ key stct
  |> \value -> groupPut_ key (op value) stct


groupNew_ =
  { curr = Dict.empty
  , chgs = Dict.empty
  }


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
  -- known to be a collection of string keyed v things.
  | Group (GroupStruct_ euser v)


{-| True if the resource is unknownResource. -}
isUnknown : Resource euser v -> Bool
isUnknown res =
  case res of
    Unknown -> True
    _ -> False


{-| False if the resource is unknownResource. -}
isNotUnknown : Resource euser v -> Bool
isNotUnknown = isUnknown >> not


{-| True if the resource is pendingResource. -}
isPending : Resource euser v -> Bool
isPending res =
  case res of
    Pending -> True
    _ -> False

{-| False if the resource is pendingResource. -}
isNotPending : Resource euser v -> Bool
isNotPending = isPending >> not


{-| True if the resource is voidResource. -}
isVoid : Resource euser v -> Bool
isVoid res =
  case res of
    Void -> True
    _ -> False


{-| False if the resource is voidResource. -}
isNotVoid : Resource euser v -> Bool
isNotVoid = isVoid >> not


{-| True if the resource is unknownResource or voidResource. -}
isNil : Resource euser v -> Bool
isNil res =
  case res of
    Unknown -> True
    Void -> True
    _ -> False


{-| False if the resource is unknownResource or voidResource. -}
isNotNil : Resource euser v -> Bool
isNotNil = isNil >> not


{-| True if the resource is undecidedResource. -}
isUndecided : Resource euser v -> Bool
isUndecided res =
  case res of
    Undecided _ -> True
    _ -> False


{-| False if the resource is undecidedResource. -}
isNotUndecided : Resource euser v -> Bool
isNotUndecided = isUndecided >> not


{-| True if the resource is forbiddenResource. -}
isForbidden : Resource euser v -> Bool
isForbidden res =
  case res of
    Forbidden _ -> True
    _ -> False


{-| False if the resource is forbiddenResource. -}
isNotForbidden : Resource euser v -> Bool
isNotForbidden = isForbidden >> not


{-| True if the resource is a pending operation. -}
isOperation : Resource euser v -> Bool
isOperation res =
  case res of
    Operation _ -> True
    _ -> False


{-| False if the resource is a pending operation. -}
isNotOperation : Resource euser v -> Bool
isNotOperation = isOperation >> not


{-| True if the resource is known. -}
isKnown : Resource euser v -> Bool
isKnown res =
  case res of
    Known _ -> True
    _ -> False


{-| False if the resource is known. -}
isNotKnown : Resource euser v -> Bool
isNotKnown = isKnown >> not


{-| True if the resource is unknownResource. -}
isGroup : Resource euser v -> Bool
isGroup res =
  case res of
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


{-| Transform a `ResourceTask` back in to a `UserTask`, which will produce a nested `Resource`
finger reflecting the final path output of the given `ResourceTask`. This output resource can be
treated just like a delta using the `Resource.merge` and `Resource.mergeMany` functions to inject it
in to the working set.  -}
deltaTask : ResourceTask euser v -> UserTask euser v
deltaTask optask =
  optask
    `andThen` (\{resource, path} -> prefixPath path resource |> Task.succeed)
    `onError` (\(path', err') -> undecidedResource err' |> prefixPath path' |> Task.succeed)


{-| Convert a `UserTask euser v` in to an `App.ProgramTask bad a` -}
toProgramTask : (Error.Error euser -> List a) -> (Resource euser v -> List a) -> UserTask euser v -> ProgramTask bad a
toProgramTask errorActions resourceActions =
  programAgent
    (resourceActions >> programAgentSuccess)
    (errorActions >> programAgentSuccess)


{-| The equivalent of therefore for ResourceTasks. This allows you to map fetched data to multiple
models with ease, as long as operations which should effect all of the models are all sunk in to
ResourceTasks producing the base model's type. -}
comprehend : (v -> v') -> ResourceTask euser v -> ResourceTask euser v'
comprehend xdcr optask =
  optask `andThen` (\ref -> { ref | resource = therefore xdcr ref.resource } |> Task.succeed)


{-| Interpret the given `ResourceTask`'s resource output by a given transform function. NOTE that this
can be literally any function whose signature ends in `ResourceTask euser v -> ResourceTask euser v'`,
which is of course inclusive of `ResourceTask euser v -> ResourceTask euser v'` in the case that
`v` is the same type as `v'`. -}
interpret : (Resource euser v -> Resource euser v') -> ResourceTask euser v -> ResourceTask euser v'
interpret f optask =
  optask `andThen` (\ref -> { ref | resource = f ref.resource } |> Task.succeed)


{-| Route the results of a given `ResourceTask` to a given `ResourcePath`. -}
routeTo : ResourcePath -> ResourceTask euser v -> ResourceTask euser v
routeTo path' optask =
  optask `andThen` (\ref -> { ref | path = path' } |> Task.succeed)


{-| Provide a decider that turns an error of type `Error.Error euser` in to a resource of `Resource euser v`. -}
catchError : (Error.Error euser -> Resource euser v) -> ResourceTask euser v -> ResourceTask euser v
catchError decider optask =
  optask `onError` (\(path', err') -> Task.succeed { path = path', resource = decider err' })


{-| Collapses a resource tree made of group resources in to a single resource of the same type. -}
collapse : (List (ResourcePath, Resource euser v) -> Resource euser v) -> Resource euser v -> Resource euser v
collapse f res =
  collapse_ f [] res


collapse_ : (List (ResourcePath, Resource euser v) -> Resource euser v) -> List String -> Resource euser v -> Resource euser v
collapse_ frecurse rpath res =
  let
    cfold key res ls = (List.reverse rpath, collapse_ frecurse (key :: rpath) res) :: ls

  in
    case res of
      Group stct -> groupStructFoldr_ cfold [] stct |> frecurse
      _ -> res


{-| Flatten the given resource if it is a group to a resource of the same type. Note that unlike
`collapse`, this function does not recursively collapse the entire tree automatically. This grants
a greater degree of flexibility.  -}
flatten : (List (String, Resource euser v) -> Resource euser v) -> Resource euser v -> Resource euser v
flatten fgrp res =
  let
    cfold key res ls = (key, res) :: ls

  in
    case res of
      Group stct -> groupStructFoldr_ cfold [] stct |> fgrp
      _ -> res


{-| Variant of `flatten` whose argument function takes a dictionary instead of a list of pairs. -}
flattenDict : (Dict String (Resource euser v) -> Resource euser v) -> Resource euser v -> Resource euser v
flattenDict fgrp res =
  case res of
    Group stct ->
      stct
      |> groupStructChanged_
      |> .curr
      |> fgrp

    _ -> res


{-| Use the given function to transform all leaf resources throughout a group structure. This
applies to the result of any pending resource operations. -}
throughout : (Resource euser v -> Resource euser v) -> Resource euser v -> Resource euser v
throughout f res =
  case res of
    Group stct ->
      groupStructMap_ (throughout f) stct
      |> Group

    Operation optask -> Operation (interpret (throughout f) optask)

    _ -> f res


{-| Use the given function to transform all leaf resources throughout a group structure. This version
applies the transformation function now, even if the resource is a pending operation. This should be
used in contexts where we are rendering some resulting view of the resources most of the time. -}
throughoutNow : (Resource euser v -> Resource euser v) -> Resource euser v -> Resource euser v
throughoutNow f res =
  case res of
    Group stct ->
      groupStructMap_ (throughout f) stct
      |> Group

    _ -> f res


{-| Given a resource of value type v, create a resource of value type v' by transforming the
known value or group using some function (v -> v'). NOTE that this will create an entirely new
resouce structure, and thus any pending changes will be integrated immediately. If you wish to
preserve deltas for the purpose of mirroring and efficient data flow, then one should be using
deltaTo in order to transform just the changes. -}
therefore : (v -> v') -> Resource euser v -> Resource euser v'
therefore xdcr res =
  case res of
    Unknown -> Unknown
    Pending -> Pending
    Void -> Void

    Undecided err' -> Undecided err'
    Forbidden err' -> Forbidden err'
    Known x' -> Known (xdcr x')

    Operation optask -> Operation (comprehend xdcr optask)

    Group stct -> { curr = groupStructMapped_ (therefore xdcr) stct, chgs = Dict.empty } |> Group


{-| DEPRECIATED version of therefore, not supporting type transformation. -}
within : (sub -> sub) -> Resource euser sub -> Resource euser sub
within = therefore


{-| Manipulate an item at the given path, or else do nothing if the path does not exist. -}
atPath : (Resource euser v -> Resource euser v) -> List String -> Resource euser v -> Resource euser v
atPath operation path res =
  case path of
    [ ] -> operation res
    element :: path' ->
      case res of
        Group stct ->
          groupUpdate_ element (atPath operation path') stct
          |> Group

        _ -> res


{-| Put a resource in to a group resource at the given path. -}
putPath : (Resource euser v -> Resource euser v -> Resource euser v) -> List String -> Resource euser v -> Resource euser v -> Resource euser v
putPath choice path res' res =
  prefixPath path res'
  |> merge choice res


{-| Collision handler for nested Resources that always chooses the left hand side. -}
chooseLeft : Resource euser v -> Resource euser v -> Resource euser v
chooseLeft x _ = x


{-| Collision handler for nested Resources that always chooses the right hand side. -}
chooseRight : Resource euser v -> Resource euser v -> Resource euser v
chooseRight _ x = x


{-| Collision handler for nested Resources that voids collision keys. -}
chooseVoid : Resource euser v -> Resource euser v -> Resource euser v
chooseVoid _ _ = Void


{-| Collision handler that removes collision keys entirely. -}
chooseNeither : Resource euser v -> Resource euser v -> Resource euser v
chooseNeither _ _ = Unknown


{-| Create a path before the given resource. This has the effect of prefixing whatever is there
whether concrete or a group with the given path. Thus, creating a resource path ["foo", "bar"] and
another at ["foo", "baz"] would result in two resources that can be merged without conflicts
guaranteed because their contents are in the `foo -> bar -> ...` and `foo -> baz -> ...` subtries
respectively. -}
prefixPath : List String -> Resource euser v -> Resource euser v
prefixPath path res =
  List.foldr
    (\element res' -> Group (groupPut_ element res' groupNew_))
    res
    path


{-| Merge can be used to assemble path fingers or existing group structures arbitrarily. A common
usage would be to put many different resources at their own prefix paths, then merge them all by
folding on this if your data structure is odd, otherwise use mergeMany if you are already working
with a list. This takes a choice function that determines the outcome of two different resources
existing at the same path, _at least one of which is concrete and not a group_. Groups merge
automatically with eachother. -}
merge : (Resource euser v -> Resource euser v -> Resource euser v) -> Resource euser v -> Resource euser v -> Resource euser v
merge choice left' right' =
  let
    groupMerge_ lhs rhs =
      let
        (rhsTail, isectDict) = Dict.partition
          (\key _ -> groupGet_ key lhs |> isUnknown)
          (groupStructChanged_ rhs |> .curr)

      in
        Dict.foldl
          (\key rhv lhs' -> merge choice (groupGet_ key lhs') rhv |> flip (groupPut_ key) lhs')
          { lhs | chgs = Dict.union rhsTail lhs.chgs }
          isectDict

  in
    case (left', right') of
      (Group leftStct, Group rightStct) -> Group (groupMerge_ leftStct rightStct)
      (_, _) -> choice left' right'


{-| Merge many folds from the left over the given list of resources with merge. -}
mergeMany : (Resource euser v -> Resource euser v -> Resource euser v) -> List (Resource euser v) -> Resource euser v
mergeMany choice gs =
  case gs of
    g :: gs' ->
      List.foldl (merge choice) g gs'

    [ ] -> Void


{-| Get the item at the given path. Returns unknownResource if the item _might_ exist, but the hierarchy
does not show knowledge at the fringe (i.e., the fringe is unknown at the last known location in
the path), but may also return voidResource to a path which is known not to exist. For example,
if foo is a resource, then foo/bar cannot be a valid path because foo is not a collection. Pending
will be given in the case that an operation is pending. -}
getPath : List String -> Resource euser v -> Resource euser v
getPath path res =
  case path of
    [ ] -> res
    element :: path' ->
      case res of
        Group stct ->
          groupGet_ element stct
          |> getPath path'

        Known x' -> Void
        Void -> Void

        Pending -> Pending
        Operation optask -> Pending

        _ -> Unknown


{-| Offer a decision on some `undecidedResource res`. Undecided resource is the result of some
problem which may or may not be in control of the client. Such resource may be the result of
anything that can result in an error in your application. If this resource is an operation, then
the assumption will be applied to the result of that operation. -}
decideBy : (Error.Error euser -> Resource euser v) -> Resource euser v -> Resource euser v
decideBy decider res =
  case res of
    Undecided err' -> decider err'
    Operation optask -> Operation (catchError decider optask)

    _ -> res


{-| If some predicate `satisfies` is satisfied by the resource `res`, then we make the following
assumption. If this resource is an operation, then the assumption will be applied to
the result of that operation. -}
assumeIf : (Resource euser v -> Bool) -> v -> Resource euser v -> Resource euser v
assumeIf satisfies assume res =
  case res of
    Operation optask ->
      Operation (optask
        `andThen` (\ref -> { ref | resource = assumeIf satisfies assume ref.resource } |> Task.succeed))

    _ ->
      if satisfies res then therefore (always assume) res else res


{-| Negation of assumeIf. -}
assumeIfNot : (Resource euser v -> Bool) -> v -> Resource euser v -> Resource euser v
assumeIfNot satisfies assume res =
  assumeIf (satisfies >> not) assume res


{-| If `possibleAssumption` yields some value `value'` when a Resource is applied, then that
value is used to overwrite the resource with an assumption `Known value'`, otherwise the Resource
is unaffected. If this resource is an operation, then the assumption will be applied conditionally
to the result of that operation. -}
assumeInCase : (Resource euser v -> Maybe v) -> Resource euser v -> Resource euser v
assumeInCase possibleAssumption res =
  case res of
    Operation optask ->
      Operation (optask
        `andThen` (\ref -> { ref | resource = assumeInCase possibleAssumption ref.resource } |> Task.succeed))

    _ ->
      Maybe.map Known (possibleAssumption res)
      |> Maybe.withDefault res


{-| If some predicate `satisfies` is satisfied by the resource `res`, then we make the following
optask. -}
dispatchIf : (Resource euser v -> Bool) -> ResourceTask euser v -> Resource euser v -> Resource euser v
dispatchIf satisfies optask res =
  case res of
    Operation optask ->
      Operation (optask
        `andThen` (\ref -> { ref | resource = dispatchIf satisfies optask ref.resource } |> Task.succeed))

    _ ->
      dispatchInCase (if satisfies res then always (Just optask) else always Nothing) res


{-| Negation of dispatchIf -}
dispatchIfNot : (Resource euser v -> Bool) -> ResourceTask euser v -> Resource euser v -> Resource euser v
dispatchIfNot satisfies optask res =
  dispatchIf (satisfies >> not) optask res


{-| If `possibleOperation` yields some ResourceTask task `optask` when a Resource is applied, then
the resource is replaced by the resource `operationResource optask`, otherwise the resource is
unaffected. If this resource is an operation, then the result of that operation will be used as
the input to the provided function. In this way, operations can be chained arbitrarily deep,
but in a manner that helpfully abstracts away whether we are still waiting or already have the
result in the composition. -}
dispatchInCase : (Resource euser v -> Maybe (ResourceTask euser v)) -> Resource euser v -> Resource euser v
dispatchInCase possibleOperation res =
  case res of
    Operation optask ->
      Operation (optask
        `andThen` (\ref -> { ref | resource = dispatchInCase possibleOperation ref.resource } |> Task.succeed))

    _ ->
      Maybe.map operationResource (possibleOperation res)
      |> Maybe.withDefault res


{-| If the predicate is satisfied, apply the given transformation function. If this is a pending
operationResource, then apply deriveIf with the same arguments to the result. -}
deriveIf : (Resource euser v' -> Bool) -> (Resource euser v' -> Resource euser v') -> Resource euser v' -> Resource euser v'
deriveIf satisfies f res =
  case res of
    Operation optask ->
      Operation (optask
        `andThen` (\ref -> { ref | resource = deriveIf satisfies f ref.resource } |> Task.succeed))

    _ ->
      deriveIfNow satisfies f res


{-| If the predicate is satisfied, apply the given transformation function. -}
deriveIfNow : (Resource euser v' -> Bool) -> (Resource euser v' -> Resource euser v') -> Resource euser v' -> Resource euser v'
deriveIfNow satisfies f res' =
  if satisfies res' then f res' else res'


-- NOTE : These primitives force a reduction now even for an optask operation type
-- convenient conversion to a maybe after all mappings are given. This is intended for use when
-- mapping the state of the content to an actual display.

{-| If a resource is known, then give Just it's value, otherwise Nothing. -}
maybeKnownNow : Resource euser v' -> Maybe v'
maybeKnownNow res' =
  case res' of
    Known x' -> Just x'
    _ -> Nothing


{-| If the predicate is satisfied, replace the resource with some known value. -}
assumeIfNow : (Resource euser v' -> Bool) -> v' -> Resource euser v' -> Resource euser v'
assumeIfNow satisfies assumption res' =
  if satisfies res' then Known assumption else res'


{-| This is the counterpart to assumeInCase which does _not_ abstract away whether or not this is
some pending optask operation. Concretely, we want this in the case that we are doing model to view
reductions because a pending operation should still have some concrete visible representation, such
as an ajax loader symbol. Of course, one should still correctly call *Integrate so that an operation
is always a `pendingResource` by the time it gets past the `stage` step. -}
assumeInCaseNow : (Resource euser v' -> Maybe v') -> Resource euser v' -> Resource euser v'
assumeInCaseNow possibleAssumption res' =
  Maybe.map Known (possibleAssumption res')
  |> Maybe.withDefault res'


{-|  -}
dispatchInCaseNow : (Resource euser v -> Maybe (ResourceTask euser v)) -> Resource euser v -> Resource euser v
dispatchInCaseNow possibleOperation res =
  case res of
    Operation optask ->
      Operation (optask
        `andThen` (\ref -> { ref | resource = dispatchInCase possibleOperation ref.resource } |> Task.succeed))

    _ ->
      Maybe.map operationResource (possibleOperation res)
      |> Maybe.withDefault res


{-| In the event that the given resource is not a simple `defResource`, we replace it with a different simple
resource. -}
otherwise : v' -> Resource euser v' -> v'
otherwise assumption res' =
  case res' of
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
groupResource : List (String, Resource euser v) -> Resource euser v
groupResource members =
  Group { curr = Dict.fromList members, chgs = Dict.empty }


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
dispatch res =
  dispatch_ [] res
  |> Lazy.List.toList -- crunch to a normal list at top level.


dispatch_ : List String -> Resource euser v -> LazyList (ResourceTask euser v)
dispatch_ rpath res =
  case res of
    Group stct -> groupStructFoldr_ (\key res' ls -> (dispatch_ (key :: rpath) res') +++ ls) Lazy.List.empty stct

    Operation optask ->
      Lazy.List.singleton
        (routeTo (List.reverse rpath) optask
        |> catchError undecidedResource)

    _ -> Lazy.List.empty


{-| Update the second `Resource` argument by merging the first argument's delta `Resource`. Chooses
the left `Resource` on a conflict, mechanically speaking. -}
update : Resource euser v -> Resource euser v -> Resource euser v
update = update' chooseLeft


{-| Same as `update`, but apply a list of delta resources sequentially. This resolves all conflicts
by `chooseLeft`, which favors the deltas always. -}
updateList : List (Resource euser v) -> Resource euser v -> Resource euser v
updateList = updateList' chooseLeft


{-| Same as `update`, but pass the given conflict resolution choice function to `merge` instead of
`chooseLeft`, which is the default. This allows one to make a selection as to whether or not the
given delta is still relevant. -}
update' : (Resource euser v -> Resource euser v -> Resource euser v) -> Resource euser v -> Resource euser v -> Resource euser v
update' mergeChoice = merge mergeChoice


{-| Same as `updateList`, but uses the provided conflict resolution choice function instead of
`chooseLeft` as in `updateList`. This allows one to make a selection as to whether or not the
given delta is still relevant. -}
updateList' : (Resource euser v -> Resource euser v -> Resource euser v) -> List (Resource euser v) -> Resource euser v -> Resource euser v
updateList' mergeChoice deltas res =
  mergeMany (flip mergeChoice) (res :: deltas)


{-| Given some configuration and a resource, produce a pendingResource in the case that the
resource is an operation, otherwise give the same resource. -}
integrate : Resource euser v -> Resource euser v
integrate res =
  case res of
    Group stct -> groupStructMapChange_ integrate stct |> Group
    Operation optask -> Pending
    _ -> res


{-| `deltaTo` applies the given transformation function to the pending changes to the
`Resource` structure, producing a partial structure representing only what has changed since the
last call to `integrate`. The resulting partial structure is intended for use as a delta, to be
passed to `update` for some other Resource structure. This results in a simple one-way data binding.

To introduce k-way data binding, one need only use the `interpret` function to transform the
`ResourceTask` output of the subordinate views back in to deltas that transform the origin
`Resource` structure. NOTE that this implies when one wishes for the origin structure to reflect
changes to one of it's subordinates, one must dispatch UserTasks that succeed with the intended
changes. This can be very clean as long as there is a bijection between the origin structure
and each of it's subordinates. The complexity of the mapping is of course dependent on your record
design, so one must still take care. -}
deltaTo : (Resource euser v -> Resource euser v') -> Resource euser v -> Resource euser v'
deltaTo f res =
  case res of
    Group stct ->
      Dict.foldl
        (\key res'' grp -> update (prefixPath [key] (f res'')) grp)
        (Group groupNew_)
        stct.chgs

    Operation optask -> Pending
    _ -> f res


{-| Like `deltaTo`, but without the transformation. The following equivalency holds -}
deltaOf : Resource euser v -> Resource euser v
deltaOf = deltaTo identity


{-| Convert a resource to program input. I've found that this is a very un-Elm-like way
of doing things that makes the Elm Architecture harder to stick to. If anyone else finds a
counterexample, please let me know! If it does turn out to be useful, I will complete the set. -}
toProgram : (b -> ProgramInput a b c bad) -> Resource euser b -> Resource euser (ProgramInput a b c bad)
toProgram modelInput model' = therefore modelInput model'
