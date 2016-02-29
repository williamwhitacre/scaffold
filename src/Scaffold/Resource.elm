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
  ResourceTask,

  defResource, forbiddenResource, pendingResource, undecidedResource,
  unknownResource, voidResource, operationResource,

  maybeOr, resultOr,

  assumeIf, assumeIfNot, assumeIfNow, assumeInCase, assumeInCaseNow,
  decideBy, maybeKnownNow, therefore, otherwise,

  within,

  dispatchIf, dispatchIfNot, dispatchInCase, dispatchInCaseNow,

  isUnknown, isNotUnknown,
  isPending, isNotPending,
  isUndecided, isNotUndecided,
  isForbidden, isNotForbidden,
  isVoid, isNotVoid,
  isNil, isNotNil,
  isKnown, isNotKnown,
  isOperation, isNotOperation,

  toProgram)

  where

{-| Resource system.

@docs Resource, ResourceTask, assumeIf, assumeIfNot, assumeIfNow, assumeInCase, assumeInCaseNow, decideBy, defResource, dispatchIf, dispatchIfNot, dispatchInCase, dispatchInCaseNow, forbiddenResource, isForbidden, isKnown, isNil, isNotForbidden, isNotKnown, isNotNil, isNotOperation, isNotPending, isNotUndecided, isNotUnknown, isNotVoid, isOperation, isPending, isUndecided, isUnknown, isVoid, maybeKnownNow, maybeOr, operationResource, otherwise, pendingResource, resultOr, therefore, undecidedResource, unknownResource, voidResource, within, toProgram


-}

import Scaffold.App exposing (..)
import Scaffold.Error as Error

import Signal
import Task exposing (Task, andThen, onError)

import Dict exposing (Dict)


{-| This is a Task which represents some kind of synchronization with optask data. It can also easily
be used for long running arbitrary computations, too. It produces a Gigan Error or a Resource. -}
type alias ResourceTask euser v = Task (Error.Error euser) (Resource euser v)


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


comprehend : (v -> v') -> ResourceTask euser v -> ResourceTask euser v'
comprehend xdcr optask =
  optask `andThen` (therefore xdcr >> Task.succeed)


catchError : (Error.Error euser -> Resource euser v) -> ResourceTask euser v -> ResourceTask euser v
catchError decider optask =
  optask `onError` (decider >> Task.succeed)


{-| transform the value itself, if known, producing a resource of some new value type value'.
therefores are composed on to the results of optask operations if they represent known resource or
further operations to attempt. This allows us to compose async processing stages before resource
is finally reduced to a displayed or usable result as deeply and interchangably as we want to,
provided that we always use "therefore" _first_ to lift the resource type out before listing
a sequence of simple or contingent reductions. -}
therefore : (v -> v') -> Resource euser v -> Resource euser v'
therefore xdcr kb =
  case kb of
    Unknown -> Unknown
    Pending -> Pending
    Void -> Void

    Undecided err' -> Undecided err'
    Forbidden err' -> Forbidden err'
    Known x' -> Known (xdcr x')

    Operation optask -> Operation (comprehend xdcr optask)


{-| This is for nesting operations on resource bases. For example:

    -- this'll write something at the patch foo.bar if "bar" is void.
    baseDo (within <| baseDo (inquireIf isVoid myBarWriter) "bar") "foo" myBase

This code will work on a resource base of base knowledges, so that's a nested record. The active
record pattern can be approximated like this, and I've found it extremely handy.

-}
within : (sub -> sub) -> Resource euser sub -> Resource euser sub
within operation ksub =
  case ksub of
    Operation optask -> Operation (comprehend operation optask)
    Known x -> Known (operation x)

    _ -> ksub



{-| Offer a decision on some `undecidedResource kb`. Undecided resource is the result of some
problem which may or may not be in control of the client. Such resource may be the result of
anything that can result in an error in your application. If this resource is an operation, then
the assumption will be applied to the result of that operation.
-}
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
      Operation (optask `andThen` (assumeIf satisfies assume >> Task.succeed))

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
      Operation (optask `andThen` (assumeInCase possibleAssumption >> Task.succeed))

    _ ->
      Maybe.map Known (possibleAssumption kb)
      |> Maybe.withDefault kb


{-| If some predicate `satisfies` is satisfied by the resource `kb`, then we make the following
optask. -}
dispatchIf : (Resource euser v -> Bool) -> ResourceTask euser v -> Resource euser v -> Resource euser v
dispatchIf satisfies optask kb =
  case kb of
    Operation optask ->
      Operation (optask `andThen` (dispatchIf satisfies optask >> Task.succeed))

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
      Operation (optask `andThen` (dispatchInCase possibleOperation >> Task.succeed))

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
      Operation (optask `andThen` (dispatchInCase possibleOperation >> Task.succeed))

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
  case maybeResourceTask_ kb of
    Just optask -> [ optask `onError` \err' -> Task.succeed (undecidedResource err') ]
    Nothing -> [ ]


{-| Given some configuration and a resource, produce a pendingResource in the case that the
resource is an operation, otherwise give the same resource. -}
integrate : Resource euser v -> Resource euser v
integrate kb =
  if isOperation kb then Pending else kb


{-| -}
toProgram : (b -> ProgramInput a b c bad) -> Resource euser b -> Resource euser (ProgramInput a b c bad)
toProgram modelInput model' = therefore modelInput model'


maybeResourceTask_ : Resource euser v -> Maybe (ResourceTask euser v)
maybeResourceTask_ kb =
  case kb of
    Operation optask -> Just optask
    _ -> Nothing

{-
{-- KNOWLEDGE BASE --}

-- Resource base is configured with an address to send deltas to.
-- When one requires contacting the outside information source, an Operation,
-- one possibly gets a task for each recompute of the program where that task dispatches the
-- fetch or compute operations given as optask operations. Any Operation Resource items produce their
-- respective tasks, which send their results to the ResourceBase's address. These tasks are
-- always dispatched in parallel by folding the task list by spawn ... andThen.

{-| Create a new resource base. This takes an address which accepts resource base deltas in sequence.
Currently, you are responsible for dropping irrelevant deltas. -}
base : Signal.Address (ResourceBaseDelta euser comparable v) -> ResourceBase euser comparable v
base address =
  { base = Dict.empty
  , deltas = Dict.empty
  , deltaSink = address
  , config = baseRemoteConfig_ address Undecided
  }


{-| Add an error handler to a resource base to replace the default error handler. The default error
handler simply promotes errors to undecidedResource. You may way to give a definition in your own
error handler that distinguishes between undecidedResource and forbiddenResource. No promotion to
forbiddenResource is given by default because there is not sane default behavior that covers
the general case. -}
baseErrorHandler : (Error.Error euser -> Resource euser v) -> ResourceBase euser comparable v -> ResourceBase euser comparable v
baseErrorHandler errorHandler kbase =
  { kbase
  | config = baseRemoteConfig_ kbase.deltaSink errorHandler
  }


{-| Get the resource at a given key from the resource base. -}
baseAt : comparable -> ResourceBase euser comparable v -> Resource euser v
baseAt key kbase =
  if baseMember_ key kbase.deltas then
    baseAt_ key kbase.deltas
  else
    baseAt_ key kbase.base


{-| Determine if there is some resource at a given key that is not unknown. -}
baseMember : comparable -> ResourceBase euser comparable v -> Bool
baseMember key = baseAt key >> isNotUnknown


{-| Use one of the many resource manipulation primitives on the resource at a particular location in a resource base. -}
baseDo : (Resource euser v -> Resource euser v) -> comparable -> ResourceBase euser comparable v -> ResourceBase euser comparable v
baseDo transform key kbase =
  let
    (_, kb') =
      if baseMember_ key kbase.deltas then
        baseDeltaTransformAt_ transform key kbase.deltas
      else
        baseDeltaTransformAt_ transform key kbase.base
  in
    { kbase
    | deltas = Dict.insert key kb' kbase.deltas
    }


{-| Transform a resource base delta using `therefore`. This is very useful for mapping a signal of
deltas on to multiple resource types, then maintaining several synchronized resource bases very
efficiently. -}
baseDeltaTherefore : (v -> v') -> ResourceBaseDelta euser comparable v -> ResourceBaseDelta euser comparable v'
baseDeltaTherefore xdcr (key, kb') =
  (key, therefore xdcr kb')


{-| Transform a resource base delta using any resource primitive that does not change the resource
type. This is the meat and potatoes of ResourceBase. This can be used for operations and any of the
reductions. -}
baseDeltaMap : (Resource euser v -> Resource euser v) -> ResourceBaseDelta euser comparable v -> ResourceBaseDelta euser comparable v
baseDeltaMap transform (key, kb') =
  (key, transform kb')


{-| Apply a resource base delta to the resource base. You are responsible for determining the order
of the updates. -}
baseUpdate : ResourceBaseDelta euser comparable v -> ResourceBase euser comparable v -> ResourceBase euser comparable v
baseUpdate (key, kb') kbase =
  { kbase
  | deltas = Dict.insert key kb' kbase.deltas
  }


{-| For every new operation in the resource base, get and aggregate the optask tasks, producing
Just an opaque query task or Nothing in the case that no operations need to be done. Note that this
will only traverse the resource which has changed since the last call to `baseIntegrate`, so this
scales quite well to large resource bases. -}
baseQuery : ResourceBase euser comparable v -> Maybe (QueryTask never)
baseQuery kbase =
  baseDeltaDictQuery_ kbase.config kbase.deltas


{-| Transform all new operations in to `pendingResource` across the resource base. Note that this
will only traverse the resource which has changed since the last call to `baseIntegrate`, so this
scales quite well to large resource bases. -}
baseIntegrate : ResourceBase euser comparable v -> ResourceBase euser comparable v
baseIntegrate kbase =
  { kbase
  | base = baseDeltaDictIntegrate_ kbase.deltas kbase.base
  , deltas = Dict.empty
  }


{-- KNOWLEDGE RECORD --}


{-| Create a resource record stub. The content of the resource base will be bound to the content
of the record according to the `recordField` definitions you give. A resource record stub is not
yet completed with a concrete userrecord. Use this in your definitions. Create instances of your
resource record type by using `recordSet` on the result of `record`. -}
record : Signal.Address (ResourceBaseDelta euser comparable v) -> ResourceRecordStub euser userrecord comparable v
record address =
  { kbase = base address
  , writes = Dict.empty
  , reads = Dict.empty
  }


{-| Completely replace the userrecord instance contained in a resource record or fill out a
resource record stub for the first time. The second usage is likely to be more common. -}
recordSet
  :  userrecord
  -> { k
     | kbase : ResourceBase euser comparable v
     , writes : Dict comparable (Resource euser v -> userrecord -> userrecord)
     , reads : Dict comparable (userrecord -> Resource euser v)
     }
  -> ResourceRecord euser userrecord comparable v
recordSet rec kstub =
  recordBaseSet_
    { kbase = kstub.kbase
    , writes = kstub.writes
    , reads = kstub.reads
    , record = rec
    }



{-| Map a record field to a key in the resource base. The types in the record can be any odd combination
given that the userrecord type can be anything. However, all fields in the record must have a mapping to
and from some resource type which can represent any of the fields. This is not as tricky as one might
initially surmise. Your userrecord type should have a corresponding tag union type with a tag for each
record field. To make this clean, you should have a bijection between the record fields and the tags. Here's
an example:

    type Fields = FooField Int | BarField String

    type alias MyRecord =
      { foo : Int
      , bar : String
      }

    myRecordFields =
      recordField (\k r -> { r | foo = k }) (.foo >> FooField) "foo"
      >> recordField (\k r -> { r | bar = k }) (.bar >> BarField) "bar"

    .....

    myResourceRecord =
      record address
      |> recordSet { foo = 42, bar = "answer" }
      |> myRecordFields

-}
recordField : (Resource euser v -> userrecord -> userrecord) -> (userrecord -> Resource euser v) -> comparable -> ResourceRecord euser userrecord comparable v -> ResourceRecord euser userrecord comparable v
recordField write' read' key krecord =
  { krecord
  | writes = Dict.insert key write' krecord.writes
  , reads = Dict.insert key read' krecord.reads
  }


{-| Add an error handler to a `ResourceRecord` or a `ResourceRecordStub`. Refer to
`baseErrorHandler` for a more detailed description of how adding error handlers works. -}
recordErrorHandler
  :  (Error.Error euser -> Resource euser v)
  -> { k | kbase : ResourceBase euser comparable v }
  -> { k | kbase : ResourceBase euser comparable v }
recordErrorHandler errorHandler krecord =
  { krecord
  | kbase = baseErrorHandler errorHandler krecord.kbase
  }


{-| Get the current userrecord content of a resource record. -}
recordContent : ResourceRecord euser userrecord comparable v -> userrecord
recordContent = .record


{-| Get the underlying resource base of a resource record. -}
recordBinding : ResourceRecord euser userrecord comparable v -> ResourceBase euser comparable v
recordBinding = .kbase


{-| Retrieve a resource from the resource record using some getter. Avoiding stringly-typed nonsense that you can't protect from
runtime errors is generally a good idea. Since records have an enumerable set of contant field names, it makes more sense to use
this method to get from records. This won't compile if it's wrong. For example:

    recordAt .myFieldThatWillNotCompileIfItDoesntExist myRecord

-}
recordAt : (userrecord -> Resource euser v) -> ResourceRecord euser userrecord comparable v -> Resource euser v
recordAt getter = .record >> getter


{-| Retrieve a resource at the given key. If you do this, you may as well be using resource base.
However, you will get the benefit of console debug messages in the case that you try to access a
field that doesn't exist. -}
recordAtKey : comparable -> ResourceRecord euser userrecord comparable v -> Resource euser v
recordAtKey = recordRead_


{-| This is the resource record equivalent to baseUpdate for resource bases, using the same
semantics. -}
recordUpdate : ResourceBaseDelta euser comparable v -> ResourceRecord euser userrecord comparable v -> ResourceRecord euser userrecord comparable v
recordUpdate kbdelta krecord =
  recordWrite_ kbdelta krecord


{-| This is the resource record equivalent to baseDo for resource bases, using the same
semantics. -}
recordDo : (Resource euser v -> Resource euser v) -> comparable -> ResourceRecord euser userrecord comparable v -> ResourceRecord euser userrecord comparable v
recordDo transform key krecord =
  -- easy as pie! :`-D
  recordRead_ key krecord
  |> transform
  |> \kb' -> recordWrite_ (key, kb') krecord


{-| This is the resource record equivalent to baseQuery for resource bases, using the same
semantics. -}
recordQuery : ResourceRecord euser userrecord comparable v -> Maybe (QueryTask never)
recordQuery =
  .kbase >> baseQuery


{-| This is the resource record equivalent to baseIntegrate for resource bases, using the same
semantics. -}
recordIntegrate : ResourceRecord euser userrecord comparable v -> ResourceRecord euser userrecord comparable v
recordIntegrate krecord =
  { krecord | kbase = baseIntegrate krecord.kbase }


{-| This utility function produces a program input resource from a resource of it's model, given a partial program definition (with the model omitted).
    It is redundant, but helps make the use case more concrete.

    -- Example:

    myProgramInputResource = Resource.toProgram (App.defProgram' present stage update) myModelResource


    -}
toProgram : (b -> ProgramInput a b c bad) -> Resource euser b -> Resource euser (ProgramInput a b c bad)
toProgram modelInput model' = therefore modelInput model'


-- NOTE : The following is the INTERNAL implementation of ResourceBase. Use ResourceBase and it's
-- functions (beginning with base, no underscore at the end) to manipulate resource bases.

recordBaseSet_ : ResourceRecord euser userrecord comparable v -> ResourceRecord euser userrecord comparable v
recordBaseSet_ krecord =
  Dict.foldl
    (\key fread krecord' -> recordWrite_ (key, fread krecord'.record) krecord')
    krecord
    krecord.reads


recordRWCrashMessage_ : comparable -> String
recordRWCrashMessage_ key =
  "For key " ++ (toString key) ++ " "
  ++ "there is not a complete read/write pair, which violates the constraints of the "
  ++ "symachine! ONLY USE THE PROVIDED PRIMITIVES TO MANIPULATE ResourceRecords, it is an "
  ++ "opaque type."


recordRW_ : comparable -> ResourceRecord euser userrecord comparable v -> Maybe { write : Resource euser v -> userrecord -> userrecord, read : userrecord -> Resource euser v }
recordRW_ key krecord =
  let
    arrangement write' read' =
      Just { write = write', read = read' }

  in
    Dict.get key krecord.writes
    |> \maybeWrite -> Dict.get key krecord.reads
    |> \maybeRead ->
      case (maybeWrite, maybeRead) of
        (Just write', Just read') -> Just { read = read', write = write' }
        (Nothing, Nothing) ->
          Debug.log "For ResourceRecord key" key
          |> \_ -> Debug.log "Expecting to find read and write functions but found" Nothing

        (Just _, Nothing) -> Debug.crash (recordRWCrashMessage_ key)
        (Nothing, Just _) -> Debug.crash (recordRWCrashMessage_ key)


recordWrite_ : ResourceBaseDelta euser comparable v -> ResourceRecord euser userrecord comparable v -> ResourceRecord euser userrecord comparable v
recordWrite_ (key, kb') krecord =
  recordRW_ key krecord
  |> Maybe.map (\{write} ->
    { krecord
    | kbase = baseUpdate (key, kb') krecord.kbase
    , record = write kb' krecord.record
    })
  |> Maybe.withDefault krecord


recordRead_ : comparable -> ResourceRecord euser userrecord comparable v -> Resource euser v
recordRead_ key krecord =
  recordRW_ key krecord
  |> Maybe.map (\{read} -> read krecord.record)
  |> Maybe.withDefault Unknown


baseAt_ : comparable -> BaseImpl euser comparable v -> Resource euser v
baseAt_ key kbdict =
  Dict.get key kbdict
  |> Maybe.withDefault Unknown


baseMember_ : comparable -> BaseImpl euser comparable v -> Bool
baseMember_ key kbdict =
  case baseAt_ key kbdict of
    Unknown -> False
    _ -> True


declareRemoteResultDispatch_ : RemoteConfig euser v -> ResourceTask euser v -> QueryTask never
declareRemoteResultDispatch_ config optask =
  catchError config.errorHandler optask
    `andThen` (Signal.send config.address) -- got a new Resource as a result of the Operation
    `onError` (Undecided >> Signal.send config.address) -- last ditch if the error handler erred.


remoteConfigAt_ : comparable -> RemoteMapConfig euser comparable v -> RemoteConfig euser v
remoteConfigAt_ key configMap =
  { address = configMap.addressOf key
  , errorHandler = configMap.errorHandlerOf key
  }


baseDeltaDictRemoteMap_ : RemoteMapConfig euser comparable v -> BaseImpl euser comparable v -> RemoteMap euser comparable v
baseDeltaDictRemoteMap_ configMap deltas =
  Dict.foldl
    (\key kb' resultMap -> maybeRemoteTask_ kb'
    |> Maybe.map (flip (Dict.insert key) resultMap) -- with task added
    |> Maybe.withDefault resultMap) -- unchanged
    Dict.empty
    deltas


baseDeltaIntegrate_ : BaseDeltaImpl euser comparable v -> BaseImpl euser comparable v -> BaseImpl euser comparable v
baseDeltaIntegrate_ (key, kb') kbdict0 =
  case kb' of
    Unknown   -> Dict.remove key kbdict0         -- Unknowns are removed.
    Operation _ -> Dict.insert key Pending kbdict0 -- Operation becomes pending.
    _         -> Dict.insert key kb' kbdict0     -- Anything else is updated.


baseDeltaDictIntegrate_ : BaseImpl euser comparable v -> BaseImpl euser comparable v -> BaseImpl euser comparable v
baseDeltaDictIntegrate_ deltas kbdict0 =
  Dict.foldl (curry baseDeltaIntegrate_) kbdict0 deltas


-- `baseTransformAt` can be combined by currying easily with any of the above resource translations
-- ending in (Resource euser v -> Resource euser v)
baseDeltaTransformAt_ : (Resource euser v -> Resource euser v) -> comparable -> BaseImpl euser comparable v -> BaseDeltaImpl euser comparable v
baseDeltaTransformAt_ transform key kbdict =
  Dict.get key kbdict
  |> Maybe.map transform
  |> Maybe.withDefault (transform Unknown)
  |> (,) key


remoteMapQuery_ : RemoteMapConfig euser comparable v -> RemoteMap euser comparable v -> Maybe (QueryTask never)
remoteMapQuery_ configMap remoteMap =
  let
    declareDispatch key optask =
      declareRemoteResultDispatch_ (remoteConfigAt_ key configMap) optask

    foldOperation key optask mtask =
      case mtask of
        Just task' -> Just (Task.spawn task' `andThen` \_ -> declareDispatch key optask)
        Nothing -> Just (declareDispatch key optask)
  in
    Dict.foldl
      foldOperation
      Nothing
      remoteMap


baseDeltaDictQuery_ : RemoteMapConfig euser comparable v -> BaseImpl euser comparable v -> Maybe (QueryTask never)
baseDeltaDictQuery_ configMap deltas =
  baseDeltaDictRemoteMap_ configMap deltas
  |> remoteMapQuery_ configMap



baseRemoteConfig_ : Signal.Address (BaseDeltaImpl euser comparable v) -> (Error.Error euser -> Resource euser v) -> RemoteMapConfig euser comparable v
baseRemoteConfig_ address errorHandler =
  { addressOf = (\key -> Signal.forwardTo address (\kb' -> (key, kb')))
  , errorHandlerOf = always errorHandler
  }
-}
