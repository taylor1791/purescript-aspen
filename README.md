# purescript-aspen

[![Greenkeeper badge](https://badges.greenkeeper.io/taylor1791/purescript-aspen.svg)](https://greenkeeper.io/)
[![Build status](https://travis-ci.org/taylor1791/purescript-aspen.svg?branch=master)](https://travis-ci.org/taylor1791/purescript-aspen)

Combine Redux action handlers into a type-safe reducer.

## Install

```
bower install purescript-aspen
```

## Quick start

First write some action handlers. An action handler is a function that handles
one and only one Redux action. An `Action` is of type `Action (t :: Symbol) p`
where `t` is the `type` of the action and `p` is the type of the payload. You
can write as many of these as you want. In this example I have 3

```
addTask :: State -> Action "ADD_TASK" Task -> State
addTask = ..

toggleCompleteness :: State -> Action "TOGGLE_COMPLETENESS" Id -> State
toggleCompleteness = ...

removeTask :: State -> Action "REMOVE_TASK" Id -> State
removeTask = ...
```

After we have all of the action handlers, we need to combine them in a reducer
using `createReducer`, `handle`, and `>>=>>`. This creates a reducer with the
type of all of the supported actions in the signature providing compile time
safety. All unsupported actions types will not change the state.

```
reducer
  :: forall p t r1 r2
  .  RowCons t (Action t p) r1
      ( "REMOVE_TASK" :: Action "REMOVE_TASK" Id
      , "TOGGLE_COMPLETENESS" :: Action "ADD_TASK" Id
      , "ADD_TASK" :: Action "ADD_TASK" Task
      | r2)
  => IsSymbol t
  => State -> Action t p -> State
reducer = createReducer $
  handle addTask
  >>=>> toggleCompleteness
  >>=>> removeTask
```

## Motivation

Writing reducers in Purescript is annoying, if done naïvely. All of the type
un-safety of JavaScript can reflect in the type of the reducer and branching
on `action.type` is annoying. However, we are guaranteed not to mutate the
state and produce pure functions! If we could just get rid of the problems,
life would be great. With aspen, you can.

Aspen allows you to combine action handlers into a single Redux reducer. Each
handled action is reflected in the type of the reducer making it easy to see
what each reducer handles at-a-glance. Since the Actions are typed, you cannot
have the wrong payload body. Do so, would result in a compile time error!
Actions unknown to a reducer are passed through and ignored.

This library is like [redux-actions](https://github.com/acdlite/redux-actions),
but PureScript-ian. Thank you for the inspiration.

