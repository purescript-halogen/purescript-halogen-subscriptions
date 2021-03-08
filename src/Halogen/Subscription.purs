-- | Functions and instances for creating and managing push/pull subscriptions.
module Halogen.Subscription
  ( SubscribeIO(..)
  , create
  , Listener
  , notify
  , Emitter
  , makeEmitter
  , Subscription
  , subscribe
  , unsubscribe
  ) where

import Prelude

import Data.Array (deleteBy)
import Data.Foldable (traverse_)
import Data.Functor.Contravariant (class Contravariant)
import Effect (Effect)
import Effect.Ref as Ref
import Safe.Coerce (coerce)
import Unsafe.Reference (unsafeRefEq)

-- | A paired `Listener` and `Emitter` produced with the `create` function.
type SubscribeIO a =
  { listener :: Listener a
  , emitter :: Emitter a
  }

-- | Create a paired `Listener` and `Emitter`, which can then be used with the
-- | functions and instances in this package.
-- |
-- | ```purs
-- | { emitter, listener } <- create
-- |
-- | -- Push values into the listener:
-- | notify listener "hello"
-- |
-- | -- Subscribe to outputs from the emitter with a callback:
-- | subscription <- subscribe emitter \value ->
-- |   Console.log value
-- |
-- | -- Unsubscribe at any time:
-- | unsubscribe subscription
-- | ```
create :: forall a. Effect (SubscribeIO a)
create = do
  subscribers <- Ref.new []
  pure
    { emitter: Emitter \k -> do
        Ref.modify_ (_ <> [k]) subscribers
        pure $ Subscription do
          Ref.modify_ (deleteBy unsafeRefEq k) subscribers
    , listener: Listener \a -> do
        Ref.read subscribers >>= traverse_ \k -> k a
    }

-- | An `Emitter` represents a collection of discrete occurrences of an event;
-- | conceptually, an emitter is a possibly-infinite list of values.
-- |
-- | Emitters are created from real events like timers or mouse clicks and can
-- | be combined or transformed with the functions and instances in this module.
-- |
-- | Emitters are consumed by providing a callback via the `subscribe` function.
newtype Emitter a = Emitter ((a -> Effect Unit) -> Effect Subscription)

instance functorEmitter :: Functor Emitter where
  map f (Emitter e) = Emitter \k -> e (k <<< f)

-- | Make an `Emitter` from a function which accepts a callback and returns an
-- | unsubscription function.
-- |
-- | Note: You should use `create` unless you need explicit control over
-- | unsubscription.
makeEmitter
  :: forall a
   . ((a -> Effect Unit) -> Effect (Effect Unit))
  -> Emitter a
makeEmitter = coerce

-- | Conceptually, a `Listener` represents an input source to an `Emitter`. You
-- | can push a value to its paired emitter with the `notify` function.
newtype Listener a = Listener (a -> Effect Unit)

instance contravariantListener :: Contravariant Listener where
  cmap f (Listener g) = coerce (g <<< f)

-- | Push a value to the `Emitter` paired with the provided `Listener` argument.
-- |
-- | ```purs
-- | -- Create an emitter and listener with `create`:
-- | { emitter, listener } <- create
-- |
-- | -- Then, push values to the emitter via the listener with `notify`:
-- | notify listener "hello"
-- | ```
notify :: forall a. Listener a -> a -> Effect Unit
notify (Listener f) a = f a

-- | A `Subscription` results from subscribing to an `Emitter` with `subscribe`;
-- | the subscription can be ended at any time with `unsubscribe`.
newtype Subscription = Subscription (Effect Unit)

derive newtype instance semigroupSubscription :: Semigroup Subscription
derive newtype instance monoidSubscription :: Monoid Subscription

-- | Subscribe to an `Emitter` by providing a callback to run on values produced
-- | by the emitter:
-- |
-- | ```purs
-- | -- Produce an emitter / listener pair with `create`:
-- | { emitter, listener } <- create
-- |
-- | -- Then, subscribe to the emitter by providing a callback:
-- | subscription <- subscribe emitter \emitted ->
-- |   doSomethingWith emitted
-- |
-- | -- End the subscription at any time with `unsubscribe`:
-- | unsubscribe subscription
-- | ```
subscribe
  :: forall r a
   . Emitter a
  -> (a -> Effect r)
  -> Effect Subscription
subscribe (Emitter e) k = e (void <<< k)

-- | End a subscription to an `Emitter`.
unsubscribe :: Subscription -> Effect Unit
unsubscribe (Subscription unsub) = unsub
