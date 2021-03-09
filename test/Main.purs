module Test.Main where

import Prelude

import Data.Array (replicate)
import Data.Foldable (sequence_)
import Effect (Effect)
import Effect.Ref as Ref
import Halogen.Subscription as HS
import Test.Assert (assertEqual)

main :: Effect Unit
main = do
  emittedCount <- Ref.new 0
  { emitter, listener } <- HS.create

  -- The count should increment after subscribing to the emitter with a counter.
  sub <- HS.subscribe emitter \_ -> Ref.modify_ (_ + 1) emittedCount
  _ <- sequence_ $ replicate 5 (HS.notify listener "hello")
  count0 <- Ref.read emittedCount
  assertEqual  { actual: count0, expected: 5 }

  -- The count should not change after unsubscribing from the emitter.
  HS.unsubscribe sub
  _ <- sequence_ $ replicate 5 (HS.notify listener "hello")
  count1 <- Ref.read emittedCount
  assertEqual { actual: count1, expected: 5 }
