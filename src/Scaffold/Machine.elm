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


module Gigan.Stem

  (Stem,

  stem,

  stemModel, stemReplaceModel,

  stemDispatch, stemDispatchAs,
  stemDispatchment, stemDispatchmentAs,
  stemPresent, stemPresentAs,
  stemStage, stemStageAs,
  stemUpdate, stemIntegrate,

  maybeStemModel, maybeStemReplaceModel,

  maybeStemDispatch, maybeStemDispatchAs,
  maybeStemDispatchment, maybeStemDispatchmentAs,
  maybeStemPresent, maybeStemPresentAs,
  maybeStemStage, maybeStemStageAs,
  maybeStemUpdate, maybeStemIntegrate)

  where

{-| This module builds a more usable state machine snapshot with the Elm Architecture in mind on
top of the Gigan Core.

# Definition
@docs Stem

# Constructor
@docs stem

# Get and Set the Model
@docs stemModel, stemReplaceModel

# Using Stems
@docs stemDispatch, stemDispatchAs, stemDispatchment, stemDispatchmentAs, stemPresent, stemPresentAs, stemStage, stemStageAs, stemUpdate, stemIntegrate

# Get and Set the Model of Maybe Stems
@docs maybeStemModel, maybeStemReplaceModel

# Using Maybe Stems
@docs maybeStemDispatch, maybeStemDispatchAs, maybeStemDispatchment, maybeStemDispatchmentAs, maybeStemPresent, maybeStemPresentAs, maybeStemStage, maybeStemStageAs, maybeStemUpdate, maybeStemIntegrate

-}

import Gigan.Core exposing (..)


import Set exposing (Set)
import Dict exposing (Dict)

import Time exposing (Time)


{-| A Stem is an instantaneous description of some model associated with an OrbiterInput (see Core)
that specifies how it should be updated, staged and presented. -}
type alias Stem a b c bad =
  { input : OrbiterInput a b c bad
  , state : OrbiterSnapshot a b bad
  }


{-| Create a stem from an OrbiterInput -}
stem : OrbiterInput a b c bad -> Stem a b c bad
stem input =
  { input = input
  , state = orbiterSnapshot input.model0 (dispatchTasks [])
  }


{-| Get the model of a stem. -}
stemModel : Stem a b c bad -> b
stemModel stem' =
  stem'.state.model'


{-| Create a stem with a different model from a replacement model and an original stem. -}
stemReplaceModel : b -> Stem a b c bad -> Stem a b c bad
stemReplaceModel model' stem' =
  let stem'state = stem'.state in
    { stem' | state = { stem'state | model' = model' } }

-- FLAT STEM OPERATORS

{-| Dispatch the tasks of a Stem. This function is in the depreciated family of `stemDispatch*`
functions. Use `stemDispatchment*` and `stemIntegrate` instead. -}
stemDispatch : Stem a b c bad -> (Stem a b c bad, TaskDispatchment bad a)
stemDispatch stem' =
  orbiterSnapshotDispatch stem'.state
  |> \result -> ({ stem' | state = fst result }, snd result)


{-| Get the currently waiting dispatchment. -}
stemDispatchment : Stem a b c bad -> TaskDispatchment bad a
stemDispatchment =
  stemDispatch >> snd


{-| Dispatch the tasks of a Stem using some transducer to translate the action list. This function
is in the depreciated family of `stemDispatch*` functions. Use `stemDispatchment*` and
`stemIntegrate` instead. -}
stemDispatchAs : (List a -> List a') -> Stem a b c bad -> (Stem a b c bad, TaskDispatchment bad a')
stemDispatchAs xdcr stem' =
  stemDispatch stem'
  |> \(stem'', dispatchment) -> dispatchment
  |> promoteDispatchment xdcr
  |> \dispatchment' -> (stem'', dispatchment')


{-| Get the currently waiting task dispatchment, but apply some action type transformation. -}
stemDispatchmentAs : (List a -> List a') -> Stem a b c bad -> TaskDispatchment bad a'
stemDispatchmentAs xdcr =
  stemDispatchAs xdcr >> snd


{-| Remove the currently waiting task dispatchment. This should be done only after retrieving any
possibly waiting dispatchment. -}
stemIntegrate : Stem a b c bad -> Stem a b c bad
stemIntegrate =
  stemDispatch >> fst


{-| Run the update function from the configured OrbiterInput of the Stem on the Stem's current model to produce a new model and dispatch any asynchronous tasks desired.
This takes a list of actions, the current time, and the Stem. -}
stemUpdate : List a -> Time -> Stem a b c bad -> Stem a b c bad
stemUpdate actions now stem' =
  { stem' | state = orbiterSnapshotUpdate stem'.input actions now stem'.state }


{-| Run the update function from the configured OrbiterInput of the Stem on the Stem's current model to produce a new model and dispatch any asynchronous tasks desired.
This takes an address for actions to be sent to, the current time, and the Stem. -}
stemStage : Signal.Address (List a) -> Time -> Stem a b c bad -> Stem a b c bad
stemStage address now stem' =
  { stem' | state = orbiterSnapshotStage stem'.input address now stem'.state }


{-| Same as stemStage, but with a transformation to apply to action lists before they are sent to the address. -}
stemStageAs : (List a -> List a') -> Signal.Address (List a') -> Time -> Stem a b c bad -> Stem a b c bad
stemStageAs xdcr address =
  stemStage (Signal.forwardTo address xdcr)


{-| Run the present function from the configured OrbiterInput of the Stem on the Stem's current model to produce a ViewOutput, which includes the an element of the Stem's view type, and a TaskDispatchment for any desired asynchronous tasks.
This takes an address for actions to be sent to, the current time, and the Stem. -}
stemPresent : Signal.Address (List a) -> Time -> Stem a b c bad -> ViewOutput a c bad
stemPresent address now stem' =
  orbiterSnapshotPresent stem'.input address now stem'.state


{-| Same as stemPresent, but with a transformation to apply to action lists before they are sent to the address. -}
stemPresentAs : (List a -> List a') -> Signal.Address (List a') -> Time -> Stem a b c bad -> ViewOutput a' c bad
stemPresentAs xdcr address now stem' =
  stemPresent (Signal.forwardTo address xdcr) now stem'
  |> \view' -> { view' | dispatchment = promoteDispatchment xdcr view'.dispatchment }


{-| -}
maybeStemModel : Maybe (Stem a b c bad) -> Maybe b
maybeStemModel =
  Maybe.map (.state >> .model')


{-| -}
maybeStemReplaceModel : b -> Maybe (Stem a b c bad) -> Maybe (Stem a b c bad)
maybeStemReplaceModel model' =
  Maybe.map (stemReplaceModel model')


{-| -}
maybeStemDispatch : Maybe (Stem a b c bad) -> Maybe (Stem a b c bad, TaskDispatchment bad a)
maybeStemDispatch =
  Maybe.map stemDispatch


{-| -}
maybeStemDispatchment : Maybe (Stem a b c bad) -> TaskDispatchment bad a
maybeStemDispatchment stem' =
  Maybe.map stemDispatchment stem'
  |> Maybe.withDefault (dispatchTasks [])


-- variant of stemDispatch that accepts a transducer with which to promote actions.
{-| -}
maybeStemDispatchAs : (List a -> List a') -> Maybe (Stem a b c bad) -> Maybe (Stem a b c bad, TaskDispatchment bad a')
maybeStemDispatchAs xdcr =
  Maybe.map (stemDispatchAs xdcr)


{-| -}
maybeStemDispatchmentAs : (List a -> List a') -> Maybe (Stem a b c bad) -> TaskDispatchment bad a'
maybeStemDispatchmentAs xdcr stem' =
  Maybe.map (stemDispatchmentAs xdcr) stem'
  |> Maybe.withDefault (dispatchTasks [])


{-| -}
maybeStemIntegrate : Maybe (Stem a b c bad) -> Maybe (Stem a b c bad)
maybeStemIntegrate =
  Maybe.map stemIntegrate


{-| -}
maybeStemUpdate : List a -> Time -> Maybe (Stem a b c bad) -> Maybe (Stem a b c bad)
maybeStemUpdate actions now =
  Maybe.map (stemUpdate actions now)


{-| -}
maybeStemStage : Signal.Address (List a) -> Time -> Maybe (Stem a b c bad) -> Maybe (Stem a b c bad)
maybeStemStage address now =
  Maybe.map (stemStage address now)


{-| -}
maybeStemStageAs : (List a -> List a') -> Signal.Address (List a') -> Time -> Maybe (Stem a b c bad) -> Maybe (Stem a b c bad)
maybeStemStageAs xdcr address now =
  Maybe.map (stemStageAs xdcr address now)


{-| -}
maybeStemPresent : Signal.Address (List a) -> Time -> Maybe (Stem a b c bad) -> Maybe (ViewOutput a c bad)
maybeStemPresent address now =
  Maybe.map (stemPresent address now)


{-| -}
maybeStemPresentAs : (List a -> List a') -> Signal.Address (List a') -> Time -> Maybe (Stem a b c bad) -> Maybe (ViewOutput a' c bad)
maybeStemPresentAs xdcr address now =
  Maybe.map (stemPresentAs xdcr address now)
