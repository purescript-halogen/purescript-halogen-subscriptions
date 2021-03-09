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
  , fold
  , filter
  , fix
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Apply (lift2)
import Control.Plus (class Plus)
import Data.Array (deleteBy)
import Data.Foldable (traverse_)
import Data.Functor.Contravariant (class Contravariant)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Safe.Coerce (coerce)
import Unsafe.Reference (unsafeRefEq)

-- | A paired `Listener` and `Emitter` produced with the `create` function.
type SubscribeIO a =
  { listener :: Listener a
  , emitter :: Emitter a
  }

-- | Create a paired `Listener` and `Emitter`, where you can push values to
-- | the listener and subscribe to values from the emitter.
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

instance applyEmitter :: Apply Emitter where
  apply (Emitter e1) (Emitter e2) = Emitter \k -> do
    latestA <- Ref.new Nothing
    latestB <- Ref.new Nothing
    Subscription c1 <- e1 \a -> do
      Ref.write (Just a) latestA
      Ref.read latestB >>= traverse_ (k <<< a)
    Subscription c2 <- e2 \b -> do
      Ref.write (Just b) latestB
      Ref.read latestA >>= traverse_ (k <<< (_ $ b))
    pure (Subscription (c1 *> c2))

instance applicativeEmitter :: Applicative Emitter where
  pure a = Emitter \k -> do
    k a
    pure (Subscription (pure unit))

instance altEmitter :: Alt Emitter where
  alt (Emitter f) (Emitter g) = Emitter \k -> do
    Subscription c1 <- f k
    Subscription c2 <- g k
    pure (Subscription (c1 *> c2))

instance plusEmitter :: Plus Emitter where
  empty = Emitter \_ -> pure (Subscription (pure unit))

instance alternativeEmitter :: Alternative Emitter

instance semigroupEmitter :: Semigroup a => Semigroup (Emitter a) where
  append = lift2 append

instance monoidEmitter :: Monoid a => Monoid (Emitter a) where
  mempty = Emitter mempty

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

-- | Fold over values received from some `Emitter`, creating a new `Emitter`.
fold :: forall a b. (a -> b -> b) -> Emitter a -> b -> Emitter b
fold f (Emitter e) b = Emitter \k -> do
  result <- Ref.new b
  e \a -> Ref.modify (f a) result >>= k

-- | Create an `Emitter` which only fires when a predicate holds.
filter :: forall a. (a -> Boolean) -> Emitter a -> Emitter a
filter p (Emitter e) = Emitter \k -> e \a -> if p a then k a else pure unit

-- | Compute a fixed point.
fix :: forall i o. (Emitter i -> { input :: Emitter i, output :: Emitter o }) -> Emitter o
fix f = Emitter \k -> do
  Subscription c1 <- subscribe input (notify listener)
  Subscription c2 <- subscribe output k
  pure (Subscription (c1 *> c2))
  where
  { emitter, listener } = unsafePerformEffect create
  { input, output } = f emitter
