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

type SubscribeIO a =
  { listener :: Listener a
  , emitter :: Emitter a
  }

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

newtype Listener a = Listener (a -> Effect Unit)

instance contravariantListener :: Contravariant Listener where
  cmap f (Listener g) = Listener (g <<< f)

notify :: forall a. Listener a -> a -> Effect Unit
notify (Listener f) a = f a

newtype Emitter a = Emitter ((a -> Effect Unit) -> Effect Subscription)

instance functorEmitter :: Functor Emitter where
  map f (Emitter e) = Emitter \k -> e (k <<< f)

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
