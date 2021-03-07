module Halogen.Emitter
  ( EmitIO(..)
  , Listener
  , notify
  , Emitter
  , create
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

type EmitIO a =
  { listener :: Listener a
  , emitter :: Emitter a
  }

newtype Listener a = Listener (a -> Effect Unit)

instance contravariantListener :: Contravariant Listener where
  cmap f (Listener g) = Listener (g <<< f)

notify :: forall a. a -> Listener a -> Effect Unit
notify a (Listener f) = f a

newtype Emitter a = Emitter ((a -> Effect Unit) -> Effect Subscription)

instance functorEmitter :: Functor Emitter where
  map f (Emitter e) = Emitter \k -> e (k <<< f)

create :: forall a. Effect (EmitIO a)
create = do
  subscribers <- Ref.new []
  pure
    { emitter: Emitter \k -> do
        _ <- Ref.modify (_ <> [k]) subscribers
        pure $ Subscription do
          _ <- Ref.modify (deleteBy unsafeRefEq k) subscribers
          pure unit
    , listener: Listener \a -> do
        Ref.read subscribers >>= traverse_ \k -> k a
    }

makeEmitter
  :: forall a
   . ((a -> Effect Unit) -> Effect (Effect Unit))
  -> Emitter a
makeEmitter = coerce

newtype Subscription = Subscription (Effect Unit)

derive newtype instance semigroupSubscription :: Semigroup Subscription
derive newtype instance monoidSubscription :: Monoid Subscription

subscribe
  :: forall r a
   . Emitter a
  -> (a -> Effect r)
  -> Effect Subscription
subscribe (Emitter e) k = e (void <<< k)

unsubscribe :: Subscription -> Effect Unit
unsubscribe (Subscription unsub) = unsub
