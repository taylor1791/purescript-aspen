-- | A working aspen tutorial! Oh and a test too. Read the source.

module Test.Main where

import Prelude (class Eq, Unit, ($), (==), (>>=), not, discard)
import Control.Monad.Eff (Eff)
import Data.Array (snoc, findIndex, alterAt, deleteBy)
import Data.Generic (class Generic, gEq)
import Data.Maybe (Maybe(..))
import Test.Assert (ASSERT, assert')

import Aspen
  (Action(..), ActionCreator,
  createAction, createReducer, handle, (>>=>>))

-- Let's build the model of an app. Everyone likes todo apps, so lets do that!
-- An entry in the todo list will have a unique id, a flag if it is completed,
-- and the entered text. We will use newtype so that it compiles down to a POJO
-- for Redux serialization.
type Id   = Int
type TaskRecord = {id :: Id, completed :: Boolean, item :: String}
newtype Task    = Task TaskRecord

-- Now, the state for the app. We need to know the users list of todos.
newtype State = State {todos :: Array Task}

-- Allow for easy comparison with Eq
derive instance genericTask :: Generic Task
instance eqTask :: Eq Task where
  eq = gEq

derive instance genericState :: Generic State
instance eqState :: Eq State where
  eq = gEq

-- This is the hardest part. We define one function for each action we want to
-- handle. Here we have actions to add a task, toggle completeness, and delete
-- a task. An `Action t p` is just an alias for {"type": String, payload :: p}
-- where `t` is a phantom type for saftey.

addTask :: State -> Action "ADD_TASK" Task -> State
addTask (State state@{todos}) (Action {payload}) =
  State $ state {todos = snoc todos payload}

toggleCompleteness :: State -> Action "TOGGLE_COMPLETENESS" Id -> State
toggleCompleteness state@(State rec@{todos}) (Action {payload}) =
  case mIndex >>= mResult of
    Just todos' -> State $ rec {todos = todos'}
    Nothing     -> state
    where
      hasId id (Task task) = task.id == id
      mIndex = findIndex (hasId payload) todos
      update (Task task) = Just $ Task $ task {completed = not task.completed}
      mResult i = alterAt i update todos

removeTask :: State -> Action "REMOVE_TASK" Id -> State
removeTask state@(State rec@{todos}) (Action {payload}) =
  State $ rec {todos = deleteBy sameId isoTodo todos}
    where
      isoTodo = Task $ {id: payload, completed: false, item: ""}
      sameId (Task t1) (Task t2) = t1.id == t2.id

-- Now we define some action creators, so we don't have to annotate the use
-- of `createAction` later. `ActionCreator t p` is just an alias for
-- `p -> Action t p`. This section is optional
addTaskAction :: ActionCreator "ADD_TASK" Task
addTaskAction = createAction

toggleCompletenessAction :: ActionCreator "TOGGLE_COMPLETENESS" Id
toggleCompletenessAction = createAction

removeTaskAction :: ActionCreator "REMOVE_TASK" Id
removeTaskAction = createAction

-- Finally we can combine our action handlers into a single reducer. Notice
-- that all the type of actions that the reducer can handle are built into
-- the type.

-- TODO I would like to have a type signature here, but using the type given
-- by purs produces an errors about "the instance head [containing] unknown
-- type variables."
-- reducer
--   :: forall p t r1 r2
--   .  RowCons t (Action t p) r1
--       ( "REMOVE_TASK" :: Action "REMOVE_TASK" Id
--       , "TOGGLE_COMPLETENESS" :: Action "ADD_TASK" Id
--       , "ADD_TASK" :: Action "ADD_TASK" Task
--       | r2)
--   => IsSymbol t
--   => State -> Action t p -> State
reducer = createReducer $
  handle addTask
  >>=>> toggleCompleteness
  >>=>> removeTask

-- Now lets try it out!
main :: forall e. Eff (assert :: ASSERT| e) Unit
main = do
  let initialState = State {todos: []} -- Define an initial state
  let task  = Task {id: 1, completed: false, item: "Clean up after the kraken"}
  let task' = Task {id: 1, completed: true, item: "Clean up after the kraken"}

  assert' "Ignores unhanded actions" $ initialState ==
    reducer initialState (createAction 0 :: Action "RELEASE_THE_KRAKEN" Int)
  assert' "Handles ADD_TASK" $ State ({todos: [task]}) ==
    reducer initialState (addTaskAction task)
  assert' "Handles TOGGLE_COMPLETENESS" $ State ({todos: [task']}) ==
    reducer (State $ {todos: [task]}) (toggleCompletenessAction 1)
  assert' "Handles REMOVE_TASK" $ initialState ==
    reducer (State $ {todos: [task]}) (removeTaskAction 1)
  -- assert' "Does not allow dispatching actions with the wrong payload" $
  --   reducer (State $ {todos: [task]}) (removeTaskAction "Test")

