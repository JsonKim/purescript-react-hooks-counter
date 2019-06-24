module Counter
  ( mkApp
  ) where

import Prelude

import React.Basic.DOM as R
import React.Basic.Events (EventHandler, handler_)
import React.Basic.Hooks (CreateComponent, Hook, UseState, component, element, fragment, useState, (/\))
import React.Basic.Hooks as React

type Counter =
  { value :: Int
  , increase :: EventHandler
  , decrease :: EventHandler
  , reset :: EventHandler
  }

useCounter :: Int -> Hook (UseState Int) Counter
useCounter initialCount = React.do
  count /\ setCounter <- useState initialCount
  pure
    { value: count
    , increase: handler_ $ setCounter (_ + 1)
    , decrease: handler_ $ setCounter (_ - 1)
    , reset: handler_ $ setCounter (\_ -> initialCount)
    }

mkCounter :: CreateComponent { initialCount :: Int }
mkCounter = do
  component "Counter" \{ initialCount } -> React.do
    counter <- useCounter initialCount

    pure $ fragment
      [ R.text $ "Count: " <> show counter.value
      , R.br {}
      , R.br {}
      , R.button
          { onClick: counter.reset
          , children: [ R.text "Reset" ]
          }
      , R.button
          { onClick: counter.increase
          , children: [ R.text "+" ]
          }
      , R.button
          { onClick: counter.decrease
          , children: [ R.text "-" ]
          }
      ]

mkApp :: CreateComponent {}
mkApp = do
  counter <- mkCounter

  component "App" $ const <<< pure $ R.div
    { className: "App"
    , children:
        [ R.h1_ [R.text "React Hooks Example"]
        , element counter { initialCount: 1 }
        ]
    }
