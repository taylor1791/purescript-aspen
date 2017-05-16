-- | Combine Redux action handlers into a type-safe reducer.

module Aspen
  (Action(..), ActionCreator,
  createAction, createReducer, handle, combine, (>>=>>)) where

import Prelude (flip, id)
import Data.Variant (SProxy(..), Variant, on, inj, default)
import Data.Symbol (class IsSymbol, reflectSymbol)

newtype Action (t :: Symbol) p = Action {"type" :: String, payload :: p}
type ActionCreator t p = IsSymbol t => p -> Action t p

-- | _The_ action creator. This function can create all of your action with
-- | a type annotation. None of this action creator creator stuff.
-- |
-- | ```
-- | createLoginAction :: User -> Action "LOG_IN_USER" User
-- | createLoginAction = createAction
-- |
-- | -- Or on-demand
-- | action = createAction user :: Action "LOG_IN_USER"
-- | ```
createAction
  :: forall t p
  .  IsSymbol t
  => p -> Action t p
createAction payload =
  Action {type: reflectSymbol (SProxy :: SProxy t), payload}

-- | Takes "action handler set" and creates a reducer. An action handler is a
-- | function of type `s -> Action t p -> s` where `s` is the state handled
-- | by the reducer. `t` is the type of action to handle and `p` is the type of
-- | the payload. An exmaple action handler may be of type
-- | `logIn :: State -> `Action "LOG_IN" User` -> State`.
createReducer
  :: forall t11 t15 t16 s t p
  .  RowCons t (Action t p) t16 t15
  => IsSymbol t
  => ((Variant t11 -> s -> s) -> Variant t15 -> s -> s)
  -> s -> Action t p -> s
createReducer f s a = (f (default id)) (injectAction a) s

-- | Converts a single action handler into an "action handler set"
-- | `handlers = handle logIn`. This "action handler set" can be `combine`d
-- |with other action handlers using `combine` or `>>=>>`.
handle
  :: forall s t p a a'
  .  RowCons t (Action t p) a a'
  => IsSymbol t
  => (s -> Action t p -> s)
  -> (Variant a -> s -> s)
  -> Variant a' -> s -> s
handle = combine id

-- | Given an existing "action handler set", extend it to also handle an
-- | additional action. `handers >>=>> logIn >>=>> logOut >>=>> createUser`.
-- | You can combine as many action handlers as you want. `>>=>>` is the
-- | operator for `combine`. It has precedence 4 and may change in the
-- | future. Also, note that the first action handler must "seed" the "action
-- | handler set". This is done with `handle`.
combine
  :: forall t26 s t p a a'
  .  RowCons t (Action t p) a a'
  => IsSymbol t
  => (t26 -> Variant a -> s -> s)
  -> (s -> Action t p -> s)
  -> t26 -> Variant a' -> s -> s
combine a h x = createActionHandler h (a x)

infixl 4 combine as >>=>>


createActionHandler
  :: forall s t p v1 v2
  .  RowCons t (Action t p) v1 v2
  => IsSymbol t
  => (s -> Action t p -> s) -> (Variant v1 -> s -> s) -> Variant v2 -> s -> s
createActionHandler handler = on (SProxy :: SProxy t) (flip handler)

injectAction
  :: forall t p r1 r2
  . RowCons t (Action t p) r1 r2
  => IsSymbol t
  => Action t p -> Variant r2
injectAction = inj (SProxy :: SProxy t)

