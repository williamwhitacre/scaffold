# Scaffold

Stand by.


## This is not an alternative architecture.

This scaffolding is intended to provide a set of very useful types and DSL libraries to help make your Elm Architecture code softer and more semantically understandable. It incorporates Time as an argument to each user provided function and also provides an optional intermediate `stage` function between `update` and `present` which gives us the opportunity to use memoization, any sorts of expensive view staging algorithms we would only want to do once per atomic batch of updates at the end before viewing, any sorts of view recompute optimizations.


# What's available?

### Scaffold.App

Contains basic definitions for Progam, ProgramInput, and ProgramOutput. Defines the concepts of TaskDispatchments and ProgramConnectors for routing dataflow at the top level of your program. Defines the needed functions to run programs, route input sequence signals, and route the resulting action lists from output TaskDispatchment signals to destination addresses, the self (this running Program), or error handlers.

_(In Gigan, this was formerly called Gigan.Core.)_

### Scaffold.Machine

Contains basic definition of the Machine. A Machine is just a snapshot of a Program with a really simple instruction library for stepping it, examining it, and committing it's pending TaskDispatchment. Machines are essentially a way to embed programs in other programs responsible for deferring control to them, much like coroutines. Of course, despite these synchronous semantics, we get the fully asynchronous Elm runtime treatment.

This is a good way to build modular programs, and works by adjusting all instances of tasks and actions to _lists of tasks and actions instead_. This gives us the power to provide utilities dispatching, ordering, and executing arbitrary sequences of actions with an easy way to select whether or not atomic execution of a given sequence of actions is required. What that does is it provides us with a way compose actions out of sequences of simpler actions, yielding simpler business logic inside components and a rich world of idioms to explore for the bolder among us, but it still feels and scales the same way as a StartApp application because it's fundamentally still just the Elm Architecture.

_(In Gigan, this was formerly called Gigan.Stem.)_

### Scaffold.Resource

Contains the basic definition of Resource, ResourceBase, ResourceRecord, the BaseResource and RecordResource types for nesting, and the Remote and QueryTask types for reactivity and remote synchronization. ~~A MachineResource type is a natural extension of this, and has yet to be implemented in the library.~~ See the `machineResource` function for a basic bridge that derives a machine from a resource with a one way data flow. This is really awesome for pure views, and the business logic for resource management is one level up in the controller, as it should be. This is done with resource reductions and the `base` and `record` mapping facilities. ~~Even with it not being an explicit feature of the library,~~ the result of using a ResourceBase of Machines bound to remote or locally stored data by your own CRUD tasks library for your specific back-end. It can be as rich or as basic as you need. Resources can be the following things:

1. `unknownResource` -- Something that isn't known at all. This also counts as nothing or no record in the Resource.Base and Resource.Record APIs.
2. `pendingResource` -- A task that will perform some work and give an updated version of the resource has been dispatched, and this placeholder is given in place of the pending result. Resources that are set to produce Remote Tasks will be rolled over to pending resources on `*Integrate` on a base, record or individual resource.
3. `voidResource` -- A resource that does not exist, verified. Could be thought of as NothingRemotelyOrHere.
4. `undecidedResource` -- Represent a resource resulting from a generic error. This is also used as the default promotion from Error to Resource.
5. `forbiddenResource` -- Forbidden resources should be used to denote things that the user shouldn't see.
6. `resourceDo` -- perform some task to update the resource. Can be anything, including computation, fetching, or fully blown CRUD.
7. `knownResource` -- A resource with a known and up to date datum.

You can create a mapping from data to view machines:

    deltaDataToView = machineResource (defProgram' present stage update) myViewData


Resources can be resolved compositionally:

    -- fetchMyContent is the user supplied task for getting the resource filled out.
    -- try alternative might produce an alternative according to said user defined function.
    deltaContentResource' =
      dispatchIf isUnknown fetchMyContent
      >> dispatchInCase tryAlternative
      >> decideBy myErrorPromote


Resource types can be transformed:

    myControllerResource =
      deltaContentResource' myContentResource
      |> therefore (machine (defProgram' ...))

_(In Gigan, this was formerly called Gigan.Knowledge.)_


# Can I see how to use it?

I'm working on a comprehensive set of examples. There is one working public example right now.

## Software License

Gigan is released under the BSD3 license. The full text of this license is available in the `LICENSE`
file in the source repository, and it is also available from
[here](https://opensource.org/licenses/BSD-3-Clause).
