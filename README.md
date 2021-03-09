# Halogen Subscriptions

[![CI](https://github.com/purescript-halogen/purescript-halogen-subscriptions/workflows/CI/badge.svg?branch=main)](https://github.com/purescript-halogen/purescript-halogen-subscriptions/actions?query=workflow%3ACI+branch%3Amain)
[![Release](https://img.shields.io/github/release/purescript-halogen/purescript-halogen-subscriptions.svg)](https://github.com/purescript-halogen/purescript-halogen-subscriptions/releases)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-halogen-subscriptions/badge)](https://pursuit.purescript.org/packages/purescript-halogen-subscriptions)
[![Maintainer: garyb](https://img.shields.io/badge/maintainer-garyb-teal.svg)](https://github.com/garyb)
[![Maintainer: thomashoneyman](https://img.shields.io/badge/maintainer-thomashoneyman-teal.svg)](https://github.com/thomashoneyman)

Utilities for creating and managing push-based subscriptions, inspired by the [event](https://github.com/paf31/purescript-event) library. This library is used to implement subscriptions in [Halogen](https://github.com/purescript-halogen/purescript-halogen), but it can be used independently of Halogen.

## Installation

Install `halogen-subscriptions` with [Spago](https://github.com/purescript/spago):

```sh
spago install halogen-subscriptions
```

## Quick start

The `halogen-subscriptions` library helps you create and transform push-based subscriptions. Most subscriptions follow this pattern:

1. Use the `create` function to produce a paired `Emitter` and `Listener`. An emitter is a possibly-infinite list of values that you can subscribe to, and a listener is a mechanism for pushing values to the emitter.
2. Use the `subscribe` function to subscribe to outputs from the emitter by providing a callback function to run each time a value is emitted.
3. Use the `notify` function to push values to the emitter via the listener.
4. Use the `unsubscribe` function to end a subscription to an emitter.

Here's a simple example that logs "Hello" and then "Goodbye":

```purs
module Main where

import Prelude

import Effect (Effect)
import Effect.Console as Console
import Halogen.Subscription as HS

main :: Effect Unit
main = do
  { emitter, listener } <- HS.create

  subscription <- HS.subscribe emitter \str -> Console.log str

  HS.notify listener "Hello"
  HS.notify listener "Goodbye!"

  HS.unsubscribe subscription
```

Emitters can be combined and transformed to make more sophisticated subscriptions.

## Documentation

Module documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-halogen-subscriptions).
