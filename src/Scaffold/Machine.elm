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


module Scaffold.Machine

  (Machine,

  machine,

  getModel, replaceModel,

  dispatching, dispatchingAs,
  presenting, presentingAs,
  staging, stagingAs,
  updating, integrate,

  maybeGetModel, maybeReplaceModel,

  maybeDispatching, maybeDispatchingAs,
  maybePresenting, maybePresentingAs,
  maybeStaging, maybeStagingAs,
  maybeUpdating, maybeIntegrate



  )

  where

{-| This module builds a more usable state machine snapshot with the Elm Architecture in mind on
top of the Gigan App.

# Definition
@docs Machine

# Constructor
@docs machine

# Get and Set the Model
@docs getModel, replaceModel

# Using Machines
@docs dispatching, dispatchingAs, presenting, presentingAs, staging, stagingAs, updating, integrate

# Get and Set the Model of Maybe Machines
@docs maybeGetModel, maybeReplaceModel

# Optional Machines Using Maybe
@docs maybeDispatching, maybeDispatchingAs, maybePresenting, maybePresentingAs, maybeStaging, maybeStagingAs, maybeUpdating, maybeIntegrate

# Resource Dependant Machines



-}

import Scaffold.App exposing (..)
import Scaffold.Resource as Resource exposing (Resource)

import Set exposing (Set)
import Dict exposing (Dict)

import Time exposing (Time)


{-| A Machine is an instantaneous description of some model associated with an ProgramInput (see App)
that specifies how it should be updated, staged and presented. -}
type alias Machine a b c bad =
  { input : ProgramInput a b c bad
  , state : ProgramSnapshot a b bad
  }


{-| Create a machine from an ProgramInput -}
machine : ProgramInput a b c bad -> Machine a b c bad
machine input =
  { input = input
  , state = programSnapshot input.model0 (dispatchTasks [])
  }


{-| Get the model of a machine. -}
getModel : Machine a b c bad -> b
getModel machine' =
  machine'.state.model'


{-| Create a machine with a different model from a replacement model and an original machine. -}
replaceModel : b -> Machine a b c bad -> Machine a b c bad
replaceModel model' machine' =
  let machine'state = machine'.state in
    { machine' | state = { machine'state | model' = model' } }

-- FLAT STEM OPERATORS

{-| Dispatch the tasks of a Machine. This function is in the depreciated family of `dispatch'*`
functions. Use `dispatching*` and `integrate` instead. -}
dispatch' : Machine a b c bad -> (Machine a b c bad, TaskDispatchment bad a)
dispatch' machine' =
  programSnapshotDispatch machine'.state
  |> \result -> ({ machine' | state = fst result }, snd result)


{-| Get the currently waiting dispatchment. -}
dispatching : Machine a b c bad -> TaskDispatchment bad a
dispatching =
  dispatch' >> snd


{-| Dispatch the tasks of a Machine using some transducer to translate the action list. This function
is in the depreciated family of `dispatch'*` functions. Use `dispatching*` and
`integrate` instead. -}
dispatchAs' : (List a -> List a') -> Machine a b c bad -> (Machine a b c bad, TaskDispatchment bad a')
dispatchAs' xdcr machine' =
  dispatch' machine'
  |> \(machine'', dispatchment) -> dispatchment
  |> promoteDispatchment xdcr
  |> \dispatchment' -> (machine'', dispatchment')


{-| Get the currently waiting task dispatchment, but apply some action type transformation. -}
dispatchingAs : (List a -> List a') -> Machine a b c bad -> TaskDispatchment bad a'
dispatchingAs xdcr =
  dispatchAs' xdcr >> snd


{-| Remove the currently waiting task dispatchment. This should be done only after retrieving any
possibly waiting dispatchment. -}
integrate : Machine a b c bad -> Machine a b c bad
integrate =
  dispatch' >> fst


{-| Run the update function from the configured ProgramInput of the Machine on the Machine's current model to produce a new model and dispatch any asynchronous tasks desired.
This takes a list of actions, the current time, and the Machine. -}
updating : List a -> Time -> Machine a b c bad -> Machine a b c bad
updating actions now machine' =
  { machine' | state = programSnapshotUpdate machine'.input actions now machine'.state }


{-| Run the update function from the configured ProgramInput of the Machine on the Machine's current model to produce a new model and dispatch any asynchronous tasks desired.
This takes an address for actions to be sent to, the current time, and the Machine. -}
staging : Signal.Address (List a) -> Time -> Machine a b c bad -> Machine a b c bad
staging address now machine' =
  { machine' | state = programSnapshotStage machine'.input address now machine'.state }


{-| Same as staging, but with a transformation to apply to action lists before they are sent to the address. -}
stagingAs : (List a -> List a') -> Signal.Address (List a') -> Time -> Machine a b c bad -> Machine a b c bad
stagingAs xdcr address =
  staging (Signal.forwardTo address xdcr)


{-| Run the present function from the configured ProgramInput of the Machine on the Machine's current model to produce a ViewOutput, which includes the an element of the Machine's view type, and a TaskDispatchment for any desired asynchronous tasks.
This takes an address for actions to be sent to, the current time, and the Machine. -}
presenting : Signal.Address (List a) -> Time -> Machine a b c bad -> ViewOutput a c bad
presenting address now machine' =
  programSnapshotPresent machine'.input address now machine'.state


{-| Same as presenting, but with a transformation to apply to action lists before they are sent to the address. -}
presentingAs : (List a -> List a') -> Signal.Address (List a') -> Time -> Machine a b c bad -> ViewOutput a' c bad
presentingAs xdcr address now machine' =
  presenting (Signal.forwardTo address xdcr) now machine'
  |> \view' -> { view' | dispatchment = promoteDispatchment xdcr view'.dispatchment }


{--
          OPTIONAL MACHINES USING MAYBE
--}


{-| Create a possible machine from possible model given a partial program definition (without the
model). -}
maybeMachine : (b -> ProgramInput a b c bad) -> Maybe b -> Maybe (Machine a b c bad)
maybeMachine modelInput maybeModel =
  Maybe.map (modelInput >> machine >> Just) maybeModel
  |> Maybe.withDefault Nothing


{-| -}
maybeGetModel : Maybe (Machine a b c bad) -> Maybe b
maybeGetModel =
  Maybe.map (.state >> .model')


{-| -}
maybeReplaceModel : b -> Maybe (Machine a b c bad) -> Maybe (Machine a b c bad)
maybeReplaceModel model' =
  Maybe.map (replaceModel model')


{-| -}
maybeDispatch : Maybe (Machine a b c bad) -> Maybe (Machine a b c bad, TaskDispatchment bad a)
maybeDispatch =
  Maybe.map dispatch'


{-| -}
maybeDispatching : Maybe (Machine a b c bad) -> TaskDispatchment bad a
maybeDispatching machine' =
  Maybe.map dispatching machine'
  |> Maybe.withDefault (dispatchTasks [])


-- variant of dispatch' that accepts a transducer with which to promote actions.
{-| -}
maybeDispatchAs : (List a -> List a') -> Maybe (Machine a b c bad) -> Maybe (Machine a b c bad, TaskDispatchment bad a')
maybeDispatchAs xdcr =
  Maybe.map (dispatchAs' xdcr)


{-| -}
maybeDispatchingAs : (List a -> List a') -> Maybe (Machine a b c bad) -> TaskDispatchment bad a'
maybeDispatchingAs xdcr machine' =
  Maybe.map (dispatchingAs xdcr) machine'
  |> Maybe.withDefault (dispatchTasks [])


{-| -}
maybeIntegrate : Maybe (Machine a b c bad) -> Maybe (Machine a b c bad)
maybeIntegrate =
  Maybe.map integrate


{-| -}
maybeUpdating : List a -> Time -> Maybe (Machine a b c bad) -> Maybe (Machine a b c bad)
maybeUpdating actions now =
  Maybe.map (updating actions now)


{-| -}
maybeStaging : Signal.Address (List a) -> Time -> Maybe (Machine a b c bad) -> Maybe (Machine a b c bad)
maybeStaging address now =
  Maybe.map (staging address now)


{-| -}
maybeStagingAs : (List a -> List a') -> Signal.Address (List a') -> Time -> Maybe (Machine a b c bad) -> Maybe (Machine a b c bad)
maybeStagingAs xdcr address now =
  Maybe.map (stagingAs xdcr address now)


{-| -}
maybePresenting : Signal.Address (List a) -> Time -> Maybe (Machine a b c bad) -> Maybe (ViewOutput a c bad)
maybePresenting address now =
  Maybe.map (presenting address now)


{-| -}
maybePresentingAs : (List a -> List a') -> Signal.Address (List a') -> Time -> Maybe (Machine a b c bad) -> Maybe (ViewOutput a' c bad)
maybePresentingAs xdcr address now =
  Maybe.map (presentingAs xdcr address now)



{--
          RESOURCE DEPENDANT MACHINES USING RESOURCE
--}


{-| Create a machine resource from a model resource given a partial program definition (without the
model). -}
machineResource : (b -> ProgramInput a b c bad) -> Resource euser b -> Resource euser (Machine a b c bad)
machineResource modelInput modelResource =
  Resource.toProgram modelInput modelResource
  |> Resource.therefore machine


{-| -}
resourceGetModel : Resource euser (Machine a b c bad) -> Resource euser b
resourceGetModel =
  Resource.therefore (.state >> .model')


{-| -}
resourceReplaceModel : b -> Resource euser (Machine a b c bad) -> Resource euser (Machine a b c bad)
resourceReplaceModel model' =
  Resource.therefore (replaceModel model')


{-| -}
resourceDispatching : Resource euser (Machine a b c bad) -> TaskDispatchment bad a
resourceDispatching machine' =
  Resource.therefore dispatching machine'
  |> Resource.otherwise (dispatchTasks [])


{-| -}
resourceDispatchingAs : (List a -> List a') -> Resource euser (Machine a b c bad) -> TaskDispatchment bad a'
resourceDispatchingAs xdcr machine' =
  Resource.therefore (dispatchingAs xdcr) machine'
  |> Resource.otherwise (dispatchTasks [])


{-| -}
resourceIntegrate : Resource euser (Machine a b c bad) -> Resource euser (Machine a b c bad)
resourceIntegrate =
  Resource.therefore integrate


{-| -}
resourceUpdating : List a -> Time -> Resource euser (Machine a b c bad) -> Resource euser (Machine a b c bad)
resourceUpdating actions now =
  Resource.therefore (updating actions now)


{-| -}
resourceStaging : Signal.Address (List a) -> Time -> Resource euser (Machine a b c bad) -> Resource euser (Machine a b c bad)
resourceStaging address now =
  Resource.therefore (staging address now)


{-| -}
resourceStagingAs : (List a -> List a') -> Signal.Address (List a') -> Time -> Resource euser (Machine a b c bad) -> Resource euser (Machine a b c bad)
resourceStagingAs xdcr address now =
  Resource.therefore (stagingAs xdcr address now)


{-| -}
resourcePresenting : Signal.Address (List a) -> Time -> Resource euser (Machine a b c bad) -> Resource euser (ViewOutput a c bad)
resourcePresenting address now =
  Resource.therefore (presenting address now)


{-| -}
resourcePresentingAs : (List a -> List a') -> Signal.Address (List a') -> Time -> Resource euser (Machine a b c bad) -> Resource euser (ViewOutput a' c bad)
resourcePresentingAs xdcr address now =
  Resource.therefore (presentingAs xdcr address now)
