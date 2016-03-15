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


module Scaffold.App

  (AgentStatus, ComputedResult, ComputedSuccess, ProgramInput, ProgramOutput, ProgramSnapshot,
  ProgramConnector, ProgramTask, TaskDispatchment, UpdatedModel, ViewOutput,

  defProgram, defProgram', defStagedProgram, forwardSingleton,

  run, runAnd, defSequenceInputs, defLazySequenceInputs, sink,

  updated, presented, withTasks, withDispatchment, withChildren, viewOutputTask,

  actionTask, actionTaskAsync, errorTask, computeTask, computedSuccess, computedSuccessAsync,
  noActions, nilTask,

  itself, itselfAsync, it'sErrorConnector, thisAddress, thisAddressAsync, thisForwardAddress,
  thisForwardAddressAsync, thisErrorConnector, thisForwardConnector, thisConnector,

  outputView,

  combineDispatchments, collapseTasks, dispatchTasks, dispatchmentHasWork, dispatchmentTask,
  promoteDispatchment,

  programSnapshot, programSnapshotAddDispatchment, programSnapshotDispatch,
  programSnapshotPresent, programSnapshotStage, programSnapshotUpdate, performCycle,

  agentSuccess, agentFailure, agent, successAgent, failureAgent,
  binaryAgent, blindAgent, nilAgent, resultAgent, ignoreError)

  where

{-| The main application scaffolding. You can write a really pretty top level like this:

    myProgramOutput : ProgramOutput MyAction MyModel Layout.Item Error
    myProgramOutput =
      defStagedProgram myPresent myStage myUpdate myInitialModel
      |> defSequenceInputs
        [ someVeryImportantBrowserEnvironmentInput
        , someOtherOutsideSignal
        ]
      |> runAnd (computeTask doMyComplicatedStartupWork myData)
      |> it'sErrorConnector myErrorHandler
      |> thisAddressAsync myOutsideMailboxAddress
      |> itself


    main : Signal Graphics.Element.Element
    main =
      viewOutput myProgramOutput
      |> Signal.map Layout.fromItem


    port sink : Signal (Task z ())
    port sink = sink myProgramOutput


# Definitions
@docs AgentStatus, ComputedResult, ComputedSuccess, ProgramInput, ProgramOutput, ProgramSnapshot, ProgramConnector, ProgramTask, TaskDispatchment, UpdatedModel, ViewOutput

# Define Programs
@docs defProgram, defProgram', defStagedProgram, forwardSingleton

# Run Programs
@docs run, runAnd, defLazySequenceInputs, defSequenceInputs, sink

# UpdatedModel and ViewOutput Manipulation
@docs updated, presented, withTasks, withDispatchment, withChildren, viewOutputTask

# Dispatch Actions and Errors
@docs actionTask, actionTaskAsync, errorTask, computeTask, computedSuccess, computedSuccessAsync, noActions, nilTask

# Program Output Routing and Management
@docs itself, itselfAsync, it'sErrorConnector, thisAddress, thisAddressAsync, thisForwardAddress, thisForwardAddressAsync, thisErrorConnector, thisForwardConnector, thisConnector, outputView

# Handling Tasks and TaskDispatchment
@docs combineDispatchments, collapseTasks, dispatchTasks, dispatchmentHasWork, dispatchmentTask, promoteDispatchment

# Manipulate Program Snapshots
@docs programSnapshot, programSnapshotAddDispatchment, programSnapshotDispatch, programSnapshotPresent, programSnapshotStage, programSnapshotUpdate, performCycle

# Program Task Agents
@docs agentSuccess, agentFailure, agent, successAgent, failureAgent, binaryAgent, blindAgent, nilAgent, resultAgent, ignoreError


-}

import Signal
import Signal.Extra exposing ((<~), (~>), (~))
import Debug


import Result


import Task exposing (Task, andThen, onError)
import Task.Extra


import Lazy.List exposing (LazyList, (:::), (+++))
import Time exposing (Time)


type FeedbackMethod =
  Atomically | Asynchronously


{-| A task with a ComputedSuccess result. These are used as the output task type of the Program. -}
type alias ProgramTask bad a = Task bad (ComputedSuccess a)


{-| A ComputedResult is a Result which may be a ComputedSuccess or some error type on failure. This
is used as the output type of the function passed to a `computeTask`. -}
type alias ComputedResult bad a = Result bad (ComputedSuccess a)


{-| This is the success type of an ProgramTask. It consists of a sequence of actions to execute, and
a tag saying whether we want the actions executed all at once, or spread out asynchronously. This
should be treated as opaque; use the constructors `computedSuccess` and `computedSuccessAsync`. -}
type alias ComputedSuccess a =
  { sequence : List a
  , method : FeedbackMethod
  }


{-| This is the input type for Program. It can be seen as roughly analogous to StartApp's Config
type, but carries the full configuration and input in one. This should be considered opaque. -}
type alias ProgramInput a b c bad =
  { inputs  : LazyList (Signal (LazyList a)) -- lazy list used here to reduce aggregation time
  , model0  : b
  , present : Signal.Address (List a) -> Time -> b -> ViewOutput a c bad
  , stage   : Signal.Address (List a) -> Time -> b -> UpdatedModel a b bad
  , update  : a -> Time -> b -> UpdatedModel a b bad
  }


{-| This represents an instantaneous description of an Program program. It consists of the current
model and a possible TaskDispatchment. This should be treated as opaque. -}
type alias ProgramSnapshot a b bad =
  { model' : b
  , dispatchment : TaskDispatchment bad a
  }


{-| This is the output of an Program program. This is not intended to be opaque. Any part of the
output structure is up for grabs. I do not reccomend usage of the lazyAddress externally.

    view'     Signal with the current view.
    model'    Signal with the current model.
    now       Signal with the most recent execution time.
    actions   Signal with the most recently atomically executed action list.
    tasks     Signal with the most recent ProgramTask output.
    address   The address of the Program program's Mailbox.

-}
type alias ProgramOutput a b c bad =
  { view' : Signal c
  , model' : Signal b
  , now : Signal Time
  , actions : Signal (List a) -- as a matter of robustness, we should forcibly reduce actions
                              -- action to normal lists so that no spacetime leaks occur on
                              -- account of unbounded laziness.
  , tasks : Signal (ProgramTask bad a)
  , address : Signal.Address (List a)
  , lazyAddress : Signal.Address (LazyList a) -- a performance boost where you want it,
                                              -- simplicity where it's unneeded
  }


{-| This is an opaque type representing an ordered list of tasks to execute. This is quite similar
to Effects batching. -}
type alias TaskDispatchment bad a =
  { taskExec : LazyList (ProgramTask bad a)
  }


{-| Represents the output of the `update` and `stage` functions configured in the ProgramInput.
This type exists so that TaskDispatchments and ProgramTasks can be cleanly included in the output
of `stage` and `update` without the need to return an ugly pair or embed anything in the model. -}
type alias UpdatedModel a b bad =
  { dispatchment : TaskDispatchment bad a
  , model' : b
  }


{-| Represents the output of the `present` function configured in the ProgramInput.
This type exists so that TaskDispatchments and ProgramTasks can be cleanly included in the output
of `present` without the need to return an ugly pair or embed anything in the model. -}
type alias ViewOutput a c bad =
  { dispatchment : TaskDispatchment bad a
  , view' : c
  }


{-| ProgramConnector is an alias for functions that transform ProgramTasks by routing their actions to
an address. You will probably not need to directly provide an ProgramConnector function, as the built in
family of taps should be sufficient for just about any application. -}
type alias ProgramConnector bad a =
  (Signal.Address (List a) -> ProgramTask bad a -> ProgramTask bad a)


{-| A list of no actions. -}
noActions : List a
noActions = []


{-| Produce a normal ComputedSuccess from a list of actions. These actions will be sent all at once,
and thus will be executed atomically. -}
computedSuccess : List a -> ComputedSuccess a
computedSuccess actions =
  { sequence = actions, method = Atomically }


{-| Produce an asynchronous ComputedSuccess from a list of actions. These actions will be sent
one by one from sequential tasks, and thus their execution will be spread out with no guarantee that
the sequence of actions will be executed atomically.

If you need to run a list of a few thousand actions,
and it is safe for those actions to be interspersed with other actions, you may be looking at a very
good use case for this function. Note also that this will never preempt an action list that is sent
at once using `computeSuccess`, which is a product of how the Elm runtime works. -}
computedSuccessAsync : List a -> ComputedSuccess a
computedSuccessAsync actions =
  { sequence = actions, method = Asynchronously }


{-| An ProgramTask that does nothing and produces noActions. -}
nilTask : ProgramTask bad a
nilTask = actionTask noActions


{-| An ProgramTask that carries a list of actions to execute atomically.

Note that a tap which is explicitly from the `*Async` family of taps will override this behavior,
instead producing the same behavior as actionTaskAsync. For that reason, asynchronous taps should
mainly be used for one-way data flows that are not dependent on ordering. -}
actionTask : List a -> ProgramTask bad a
actionTask actions = Task.succeed (computedSuccess actions)


{-| An ProgramTask that carries a list of actions to execute asynchronously, meaning they may be
interspersed with other feedback and inputs. -}
actionTaskAsync : List a -> ProgramTask bad a
actionTaskAsync actions = Task.succeed (computedSuccessAsync actions)


{-| An ProgramTask that carries an error. -}
errorTask : bad -> ProgramTask bad a
errorTask error' = Task.fail (error')


{-| An ProgramTask that obtains some ComputedResult from user provided data and a user provided
function, which is invoked during execution of the task. This just gives us basic support for
deferred computations. -}
computeTask : (data -> ComputedResult bad a) -> data -> ProgramTask bad a
computeTask compute' data =
  let
    computation data =
      case compute' data of
        Result.Ok result -> Task.succeed result
        Result.Err err -> Task.fail err

  in
    (Task.succeed ()) `andThen` (\_ -> computation data)


atomicSuccessConnector_ : Signal.Address (List a) -> ProgramTask bad a -> ProgramTask bad a
atomicSuccessConnector_ address task =
  let
    extractedSequence = task `andThen` (\x -> Task.succeed x.sequence)

  in
    (Task.Extra.interceptSuccess address extractedSequence)
      `andThen` actionTask


asyncSuccessConnector_ : Signal.Address (List a) -> ProgramTask bad a -> ProgramTask bad a
asyncSuccessConnector_ address task =
  let
    fitting act prior =
      prior `andThen` \_ -> atomicSuccessConnector_ address <| actionTask [act]


    spread sequenceDispatcher =
      List.foldl fitting nilTask sequenceDispatcher.sequence
  in
    task `andThen` spread
    |> Task.Extra.interceptSuccess (Signal.forwardTo address .sequence)



successConnector_ : Signal.Address (List a) -> ProgramTask bad a -> ProgramTask bad a
successConnector_ address task =
  let
    attachFitting result =
      Task.succeed result
      |> case result.method of
            Atomically -> atomicSuccessConnector_ address
            Asynchronously -> asyncSuccessConnector_ address

  in
    task `andThen` attachFitting


errorConnector_ : Signal.Address (List a) -> (bad -> List a) -> ProgramTask bad a -> ProgramTask bad a
errorConnector_ address errorActionTransform task =
  Signal.forwardTo address errorActionTransform
  |> flip Task.Extra.interceptError task


{-| This makes any error in to noActions. -}
ignoreError : bad -> List a
ignoreError = always noActions


{-| defProgram is the old declaration form for defining ProgramInput. It is simpler, and does not
support the `stage` function. Internally this uses the new form, but it has been left in for
two reasons. One is of course backwards compatibility, but sometimes you just don't need that
extra firepower, in which case it is mere clutter. -}
defProgram
  :  (Signal.Address (List a) -> Time -> b -> ViewOutput a c bad)
  -> (a -> Time -> b -> UpdatedModel a b bad)
  -> b
  -> ProgramInput a b c bad
defProgram present update model = defProgram' present (\_ _ m' -> updated m') update model


{-| defProgram' is the complete way to define an ProgramInput. ProgramInput is configured with
three functions. `update` and `present` should be familiar to users of StartApp, except for the
fact that they always have the current time. `stage` is a special addition which allows one to
use the program Mailbox address from a context in which the model can be updated.

The use case for `stage` which inspired it's existence is as follows: suppose you have a really
big model with a really, really big view. There is enough data that keen algorithms and data
structures, as well as avoiding redundant computations during presentation becomes a neccessity.

You can then structure your application as follows. Represent all these components as instances of
Machine. From inside `stage`, you can present _only the ones that will or should actually be seen_,
and cache the results in the model. Since you can create an updated model from here, you can save
anything that may be later needed during presentation.

`stage` is called only once after an entire list of actions is executed. If you get a list of 1000
actions, you can still guarantee the work in stage will be done only once, before presentation.
_The primary reason stage has access to the address is because it enables you to call present on
any sub components ahead of time for caching purposes._ -}
defProgram'
  :  (Signal.Address (List a) -> Time -> b -> ViewOutput a c bad)
  -> (Signal.Address (List a) -> Time -> b -> UpdatedModel a b bad)
  -> (a -> Time -> b -> UpdatedModel a b bad)
  -> b
  -> ProgramInput a b c bad
defProgram' present stage update model =
  { inputs = Lazy.List.empty
  , model0 = model
  , present = present
  , update = update
  , stage = stage
  }


{-| Clearer synonym for defProgram'. -}
defStagedProgram
  :  (Signal.Address (List a) -> Time -> b -> ViewOutput a c bad)
  -> (Signal.Address (List a) -> Time -> b -> UpdatedModel a b bad)
  -> (a -> Time -> b -> UpdatedModel a b bad)
  -> b
  -> ProgramInput a b c bad
defStagedProgram = defProgram'


{-| Forward applicative alternative to withSequenceInputs. -}
defSequenceInputs : List (Signal (List a)) -> ProgramInput a b c bad -> ProgramInput a b c bad
defSequenceInputs sigs inR =
  { inR
  | inputs = inR.inputs +++ (List.foldr (\x ls -> (x ~> Lazy.List.fromList) ::: ls) Lazy.List.empty sigs)
  }


{-| Forward applicative alternative to withLazySequenceInputs. -}
defLazySequenceInputs : List (Signal (LazyList a)) -> ProgramInput a b c bad -> ProgramInput a b c bad
defLazySequenceInputs sigs inR =
  { inR
  | inputs = inR.inputs +++ (List.foldr (\x ls -> x ::: ls) Lazy.List.empty sigs)
  }


{-| Turn a list of ProgramTasks in to a TaskDispatchment. -}
dispatchTasks : List (ProgramTask bad a) -> TaskDispatchment bad a
dispatchTasks = Lazy.List.fromList >> dispatchment_


{-| Turn a TaskDispatchment in to an ProgramTask. Doing this will make things a lot more opaque,
so ask yourself if it is absolutely neccessary first. Mainly this is included for completeness. -}
dispatchmentTask : TaskDispatchment bad a -> ProgramTask bad a
dispatchmentTask = reduceDispatchment_


{-| True iff the TaskDispatchment has at least one ProgramTask. -}
dispatchmentHasWork : TaskDispatchment bad a -> Bool
dispatchmentHasWork dispatchment =
  (Lazy.List.length dispatchment.taskExec) > 0


{-| Get an ProgramTask from a ViewOutput. This is DEPRECIATED. Use TaskDispatchment wherever
possible. -}
viewOutputTask : ViewOutput a c bad -> ProgramTask bad a
viewOutputTask output = output.dispatchment |> dispatchmentTask


{-| Combine two TaskDispatchment instances. This appends the task list of the second to the task
list of the first. -}
combineDispatchments : TaskDispatchment bad a -> TaskDispatchment bad a -> TaskDispatchment bad a
combineDispatchments dsp dsp' =
  { dsp | taskExec = dsp.taskExec +++ dsp'.taskExec }


{-| Using some transformation function, create a TaskDispatchment with a different action type. -}
promoteDispatchment : (List a -> List a') -> TaskDispatchment bad a -> TaskDispatchment bad a'
promoteDispatchment xdcr dsp =
  { dsp
  | taskExec =
    Lazy.List.map
      (\task -> task `andThen` \a0 -> actionTask (xdcr a0.sequence))
      dsp.taskExec
  }


{-| Add some tasks to the output of any of the three Program functions.

    updated model `withTasks` [actionTask [Jump, Run]]
    presented viewstuff `withTasks` [errorTask [reportError "You done goofed."]]

This is definitely the most elegant way to build a TaskDispatchment as well, especially in the
context of declaring causality.
-}
withTasks
  :  List (ProgramTask bad a)
  -> { anything | dispatchment : TaskDispatchment bad a }
  -> { anything | dispatchment : TaskDispatchment bad a }
withTasks tasks out' =
  { out'
  | dispatchment = dispatch_ out'.dispatchment tasks
  }


{-| This is the same as `withTasks`, but it takes an already existing TaskDispatchment. -}
withDispatchment
  : TaskDispatchment bad a
  -> { anything | dispatchment : TaskDispatchment bad a }
  -> { anything | dispatchment : TaskDispatchment bad a }
withDispatchment dispatchment out' =
  { out'
  | dispatchment = combineDispatchments out'.dispatchment dispatchment
  }


{-| This takes a list of UpdatedModel or ViewOutput instances, and appends each one to the
TaskDispatchment of the current output. This is preferred when doing model composition with Machine.
For example:

    App.staged { collectionModel | memberViews = memberOutputs }
    |> App.withChildren memberOutputs

where memberOutputs is a list of machinePresent or machinePresentAs outputs in the example.
-}
withChildren
  : List { anything | dispatchment : TaskDispatchment bad a }
  -> { anything | dispatchment : TaskDispatchment bad a }
  -> { anything | dispatchment : TaskDispatchment bad a }
withChildren children out' =
  { out'
  | dispatchment = List.foldl
      (\ch dsp -> combineDispatchments dsp ch.dispatchment)
      out'.dispatchment
      children
  }


-- ORBITER AGENTS
--
--  These primitives are designed to make it more concise to specify complex branching contingencies
-- in tasks before closing them off either with error reports (see "taps", such as it'sErrorConnector).
-- or with lists of actions to execute on success. We allow control over whether or not something is
-- interpreted as success or failure at a level that enables total intervention, so business logic
-- complexity is not limited when constructing contingency graphs (task branching). Using the Elm
-- Architecture as per usual, we can have logarithmic code size.
--

{-| Represents the status of a given ProgramAgent. Program agents are a way of transforming the
results of arbitrary tasks in to actions and errors for the program to consume. -}
type AgentStatus bad a =
  AgentSuccess (List a)
  | AgentFailure bad


{-| Successful ProgramAgent output. -}
agentSuccess : List a -> AgentStatus bad a
agentSuccess actions =
  AgentSuccess actions


{-| Failed ProgramAgent output. -}
agentFailure : bad -> AgentStatus bad a
agentFailure err' =
  AgentFailure err'


agentStatusResult status =
  case status of
    AgentSuccess ls -> Task.succeed (Result.Ok (computedSuccess ls))
    AgentFailure err' -> Task.succeed (Result.Err err')


{-| The basic program agent. Takes two functions, one of which transforms a successful result in to
an AgentStatus, and one which does the same for task failure results. This transforms an arbitrary
task in to an ProgramTask. Notice that we can easily succeed anyway even if the task failed, or vice
versa, because we get an AgentStatus which may be successful or failing either way. This means you
can skip error handling altogether if you already know what to do with the failure from the scope
you're in; you can simply map the failure on to some actions that perform an appropriate
contingency. -}
agent : (x -> AgentStatus bad a) -> (y -> AgentStatus bad a) -> Task y x -> ProgramTask bad a
agent onSuccess onFailure task =
  (task
    `andThen` (onSuccess >> agentStatusResult)
    `onError` (onFailure >> agentStatusResult))
    `andThen`
      \result ->
        case result of
          Result.Ok computedSuccess -> Task.succeed computedSuccess
          Result.Err problem -> Task.fail problem



{-| A binary program agent. This does not process any of the results, but simply always gives the
succesful AgentStatus (the first one) on task success, otherwise it gives the failed AgentStatus. -}
binaryAgent : AgentStatus bad a -> AgentStatus bad a -> Task y x -> ProgramTask bad a
binaryAgent onSuccessResult onFailureResult =
  agent (always onSuccessResult) (always onFailureResult)

{-| This is a combination of agent and programBinary agent which processes successful results
to get an AgentStatus, otherwise giving the failed agent status. -}
successAgent : (x -> AgentStatus bad a) -> AgentStatus bad a -> Task y x -> ProgramTask bad a
successAgent onSuccess onFailureResult =
  agent onSuccess (always onFailureResult)

{-| This is a combination of agent and programBinary agent which gives the successful agent
status in the case of success, and processes failed results to get an AgentStatus otherwise . -}
failureAgent : AgentStatus bad a -> (y -> AgentStatus bad a) -> Task y x -> ProgramTask bad a
failureAgent onSuccessResult onFailure =
  agent (always onSuccessResult) onFailure

{-| The other program agents defined so far are less succinct because they take two arguments, one
which applies to the success case and one which applies to the failure case. This one takes a single
function which processes the task's outcome as a Result, and so is generally a bit shorter to write. -}
resultAgent : (Result y x -> AgentStatus bad a) -> Task y x -> ProgramTask bad a
resultAgent produceResult =
  agent (Result.Ok >> produceResult) (Result.Err >> produceResult)

{-| If we don't care about the outcome of a task because it can't fail or produce a meaningful
result, we can just queue up something to do after it's done. This is perfect for using delay tasks. -}
blindAgent : AgentStatus bad a -> Task y x -> ProgramTask bad a
blindAgent result =
  binaryAgent result result

{-| No matter what, do nothing. This will get your task to run, but no kind of action or error
feedback will be produced. -}
nilAgent : Task y x -> ProgramTask bad a
nilAgent =
  blindAgent (AgentSuccess [])



dispatchment_
  :  LazyList (ProgramTask bad a)
  -> TaskDispatchment bad a
dispatchment_ taskExec =
  { taskExec = taskExec
  }


dispatch_
  :  TaskDispatchment bad a
  -> List (ProgramTask bad a)
  -> TaskDispatchment bad a
dispatch_ dispatchment seq =
  { dispatchment
  | taskExec = dispatchment.taskExec +++ (Lazy.List.fromList seq)
  }


reduceDispatchment_ : TaskDispatchment bad a -> ProgramTask bad a
reduceDispatchment_ dispatchmentRecord =
  Lazy.List.foldl
    (\task task' ->
      task
        `andThen` \a0 -> task'
        `andThen` \a1 -> actionTask (List.append a0.sequence a1.sequence)
    )
    (actionTask [])
    dispatchmentRecord.taskExec



-- TEARS OF :JOY:

{-| The sink is the final stop for ProgramOutput. This should be attached at a port to get your
tasks running. -}
sink : ProgramOutput a b c bad -> Signal (Task z ())
sink q'' =
  q''.tasks ~> \task' -> {-Debug.log "TASK RCV"-} task'
    `andThen` (\final -> {-Debug.log "TASK OK"-} final |> \_ -> Task.succeed ())
    `onError` (\err -> {-Debug.log "ERROR"-} err |> \_ -> Task.succeed ())



{--

--}

{-| Give an UpdatedModel from a model, carrying an empty TaskDispatchment. -}
updated : b -> UpdatedModel a b bad
updated model =
  { model' = model
  , dispatchment = dispatchment_ Lazy.List.empty
  }


{-| Give a ViewOutput from your view type, carrying an empty TaskDispatchment. -}
presented : c -> ViewOutput a c bad
presented view =
  { view' = view
  , dispatchment = dispatchment_ Lazy.List.empty
  }


{-| Run an program without any startup task.

    run (myProgram `withSequenceInputs` [myInput, myInput2])

-}
run : ProgramInput a b c bad -> ProgramOutput a b c bad
run defs =
  runAnd nilTask defs


{-| Run an program with a startup task
-}
runAnd : ProgramTask bad a -> ProgramInput a b c bad -> ProgramOutput a b c bad
runAnd = flip runWithWork


{-| Constructor for an ProgramSnapshot. I reccomend using the Machine module instead of these functions. -}
programSnapshot : b -> TaskDispatchment bad a -> ProgramSnapshot a b bad
programSnapshot model0 dispatchment =
  { dispatchment = dispatchment
  , model' = model0 }


{-| Add a dispatchment to an ProgramSnapshot. -}
programSnapshotAddDispatchment : TaskDispatchment bad a -> ProgramSnapshot a b bad -> ProgramSnapshot a b bad
programSnapshotAddDispatchment dispatchment' { dispatchment, model' } =
  { dispatchment = combineDispatchments dispatchment dispatchment'
  , model' = model'
  }


{-| Run the `update` phase on an ProgramSnapshot. -}
programSnapshotUpdate
  : { k | update : a -> Time -> b -> UpdatedModel a b bad }
  -> List a
  -> Time
  -> ProgramSnapshot a b bad
  -> ProgramSnapshot a b bad
programSnapshotUpdate { update } actions now state =
  let
    applyAction action state =
      update action now state.model'
      |> \updated' -> programSnapshotAddDispatchment
        updated'.dispatchment
        { state | model' = updated'.model' }

  in
    List.foldl applyAction state actions


{-
  , present : (Signal.Address (List a) -> Time -> b -> ViewOutput a c bad)
  , stage : (Signal.Address (List a) -> Time -> b -> UpdatedModel a b bad)
  , update : (a -> Time -> b -> UpdatedModel a b bad)
-}

{-| Run the `stage` phase on an ProgramSnapshot. -}
programSnapshotStage
  : { k | stage : Signal.Address (List a) -> Time -> b -> UpdatedModel a b bad }
  -> Signal.Address (List a)
  -> Time
  -> ProgramSnapshot a b bad
  -> ProgramSnapshot a b bad
programSnapshotStage { stage } address now state =
  stage address now state.model'
  |> \{dispatchment, model'} ->
    programSnapshotAddDispatchment dispatchment { state | model' = model' }


{-| Run the `present` phase on an ProgramSnapshot, yielding a ViewOutput. -}
programSnapshotPresent
  : { k | present : Signal.Address (List a) -> Time -> b -> ViewOutput a c bad }
  -> Signal.Address (List a)
  -> Time
  -> ProgramSnapshot a b bad
  -> ViewOutput a c bad
programSnapshotPresent { present } address now state =
  present address now state.model'


{-| Get the pending TaskDispatchment out of an ProgramSnapshot and clear it from the snapshot in
one go. This gives an _ugly pair_. See the newer interface for this in Machine, and also see the way
Resource works. It seems much better to separate this in to two stages. It leaves room for a mistake,
which I was trying to avoid, but it's so much cleaner than way that it's worth it and actually
leads to fewer mistakes as a result. -}
programSnapshotDispatch : ProgramSnapshot a b bad -> (ProgramSnapshot a b bad, TaskDispatchment bad a)
programSnapshotDispatch state =
  (,)
    { state
    | dispatchment = dispatchTasks []
    }
    state.dispatchment


{-| Collapse two action tasks by executing them sequentially and appending their resulting action
outputs. -}
collapseTasks : ProgramTask bad a -> ProgramTask bad a -> ProgramTask bad a
collapseTasks task task' =
  task `andThen` \a0 -> task' `andThen` \a1 -> actionTask (List.append a0.sequence a1.sequence)


{-| Perform a full cycle on an ProgramSnapshot. This looks like:

      state
      |> programSnapshotUpdate input actions now
      |> programSnapshotStage input address now
      |> programSnapshotDispatch

internally. The point of performCycle is to be used in foldp, which it is inside runWithWork
and run (which really just calls runWithWork with nilTask).
-}
performCycle
  : ProgramInput a b c bad
  -> Signal.Address (List a)
  -> (Time, List a)
  -> (ProgramSnapshot a b bad, TaskDispatchment bad a)
  -> (ProgramSnapshot a b bad, TaskDispatchment bad a)
performCycle input address (now, actions) (state, _) =
  state
  |> programSnapshotUpdate input actions now
  |> programSnapshotStage input address now
  |> programSnapshotDispatch


runWithWork : ProgramInput a b c bad -> ProgramTask bad a -> ProgramOutput a b c bad
runWithWork programInput startupTask =
  let
    { inputs, model0, present, stage, update } = programInput


    actionMailbox = Signal.mailbox Lazy.List.empty
    publicAddress = Signal.forwardTo actionMailbox.address Lazy.List.fromList


    aggregatedInputs =
      Lazy.List.foldl
        (Signal.Extra.fairMerge (+++))
        (Signal.constant Lazy.List.empty)
        (actionMailbox.signal ::: inputs)


    inputSignal = Time.timestamp (aggregatedInputs ~> Lazy.List.toList)


    initFunction input0 =
      (dispatchTasks [startupTask] |> programSnapshot model0, dispatchTasks [])
      |> performCycle programInput publicAddress input0


    programFold =
      Signal.Extra.foldp'
        (performCycle programInput publicAddress)
        initFunction
        inputSignal


    programFoldTimestamps = inputSignal ~> fst
    programFoldSnapshots = programFold ~> fst
    programFoldDispatchments = programFold ~> snd


    views = programSnapshotPresent programInput publicAddress <~ programFoldTimestamps ~ programFoldSnapshots


    combinedDispatchments =
      Signal.Extra.fairMerge
        -- model (update/stage) tasks go before view (present) tasks
        combineDispatchments
        programFoldDispatchments
        (.dispatchment <~ views)


    -- NOTE IMPORTANT: This filter is crucial to avoiding a busy loop!
    filteredTasks =
      Signal.filterMap
        (\dispatchment -> if dispatchmentHasWork dispatchment then Just (dispatchmentTask dispatchment) else Nothing)
        nilTask
        combinedDispatchments


    outputConfig =
      { view' = views ~> .view'
      , model' = programFoldSnapshots ~> .model'
      , tasks = filteredTasks
      , now = inputSignal ~> fst
      , actions = inputSignal ~> snd
      , address = publicAddress
      , lazyAddress = actionMailbox.address
      -- TODO factor the current time in to the program _output_
      }

  in
    outputConfig


{-| A tap defined using an ProgramConnector and some address. The ProgramTasks that pass through this tap will be
routed to the given address. This is a bit lower level than is needed in most cases. -}
thisConnector
  :  ProgramConnector bad a
  -> Signal.Address (List a)
  -> ProgramOutput a b c bad
  -> ProgramOutput a b c bad
thisConnector tap address output =
  { output | tasks = output.tasks ~> (tap address) }


{-| Same as thisConnector but uses Address forwarding to transform action lists in to some other target type
for your address. -}
thisForwardConnector : ProgramConnector bad a -> Signal.Address target -> (List a -> target) -> ProgramOutput a b c bad -> ProgramOutput a b c bad
thisForwardConnector tap address f output =
  thisConnector tap (Signal.forwardTo address f) output

{-| This is the most essential tap for almost any application. It routes the actions resulting from your TaskDispatchment output back to the program's main address. -}
itself : ProgramOutput a b c bad -> ProgramOutput a b c bad
itself output =
  thisConnector successConnector_ output.address output

{-| Same as itself, but execute action lists asynchronously, such that they may be interspersed with other action lists. -}
itselfAsync : ProgramOutput a b c bad -> ProgramOutput a b c bad
itselfAsync output =
  thisConnector asyncSuccessConnector_ output.address output

{-| Route the resulting actions from your TaskDispatchments to some action list address. -}
thisAddress : Signal.Address (List a) -> ProgramOutput a b c bad -> ProgramOutput a b c bad
thisAddress address output =
  thisConnector successConnector_ address output

{-| Same as thisAddress, but with forwarding. -}
thisForwardAddress : Signal.Address target -> (List a -> target) -> ProgramOutput a b c bad -> ProgramOutput a b c bad
thisForwardAddress address f output =
  thisForwardConnector successConnector_ address f output

{-| Same as thisAddress, but asynchronously as described above. -}
thisAddressAsync : Signal.Address (List a) -> ProgramOutput a b c bad -> ProgramOutput a b c bad
thisAddressAsync address output =
  thisConnector asyncSuccessConnector_ address output

{-| Same as thisForwardAddress, but asynchronously as described above. -}
thisForwardAddressAsync : Signal.Address target -> (List a -> target) -> ProgramOutput a b c bad -> ProgramOutput a b c bad
thisForwardAddressAsync address f output =
  thisForwardConnector asyncSuccessConnector_ address f output

{-| Transform any errors in to lists of actions, then route them to the address. -}
thisErrorConnector : Signal.Address (List a) -> (bad -> List a) -> ProgramOutput a b c bad -> ProgramOutput a b c bad
thisErrorConnector address handler output =
  thisConnector (flip errorConnector_ handler) address output

{-| Same semantics as `itself`, but for errors. Error taps do not have an asynchronous alternative. -}
it'sErrorConnector : (bad -> List a) -> ProgramOutput a b c bad -> ProgramOutput a b c bad
it'sErrorConnector handler output =
  thisErrorConnector output.address handler output


{-| Get the program view output. -}
outputView : ProgramOutput a b c bad -> Signal c
outputView = .view'


{-| Sometimes you don't need the fanciness of an action list address. In that case, it's cool to
not have to write your own forwarding address. -}
forwardSingleton : Signal.Address (List a) -> Signal.Address a
forwardSingleton = flip Signal.forwardTo (flip (::) [])
