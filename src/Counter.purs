module Counter
  ( mkApp
  ) where

import Prelude

import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.Events (EventHandler, handler_)
import React.Basic.Hooks (CreateComponent, Hook, UnsafeReference(..), UseCallback, UseState, component, element, fragment, useCallback, useState, (/\))
import React.Basic.Hooks as React

type UpdateState a = a -> a
type StateDispatch a = UpdateState a -> Effect Unit
type HookCallback a b = Hook (UseCallback (UnsafeReference (StateDispatch a)) (Effect Unit)) b

type Counter =
  { value :: Int
  , increase :: HookCallback Int EventHandler
  , decrease :: HookCallback Int EventHandler
  , reset :: HookCallback Int EventHandler
  }

counterCallback :: ∀ a. StateDispatch a → UpdateState a → HookCallback a (Effect Unit)
counterCallback s c = useCallback (UnsafeReference s) (s c)

handlerCallback :: ∀ a. StateDispatch a → UpdateState a → HookCallback a EventHandler
handlerCallback s c = React.do
  u <- counterCallback s c
  pure $ handler_ u

useCounter :: Int -> Hook (UseState Int) Counter
useCounter initialCount = React.do
  count /\ setCounter <- useState initialCount
  pure
    { value: count
    , increase: handlerCallback setCounter (_ + 1)
    , decrease: handlerCallback setCounter (_ - 1)
    , reset: handlerCallback setCounter (const initialCount)
    }

mkCounter :: CreateComponent { initialCount :: Int }
mkCounter = do
  component "Counter" \{ initialCount } -> React.do
    counter <- useCounter initialCount

    reset <- counter.reset
    increase <- counter.increase
    decrease <- counter.decrease

    pure $ fragment
      [ R.text $ "Count: " <> show counter.value
      , R.br {}
      , R.br {}
      , R.button
          { onClick: reset 
          , children: [ R.text "Reset" ]
          }
      , R.button
          { onClick: increase
          , children: [ R.text "+" ]
          }
      , R.button
          { onClick: decrease
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
