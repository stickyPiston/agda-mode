module Iepje.Examples.Time where

open import Iepje.Prelude
open import Iepje.Internal.JS.Language.IO
open import Iepje.Internal.JS.WebAPIs.DOM
open import Iepje.Internal.Effect

counter-effect : Effect Nat
counter-effect = from λ dispatch → do
  w ← document >>= get-defaultView
  _ ← setInterval w (λ _ → dispatch 1) 1000.0
  pure tt

time-counter : IO ⊤
time-counter = interactIO "#time-counter-app"
  (0 , counter-effect)
  (λ n → pure $ text $ (primShowNat n) ++ " seconds elapsed")
  (λ a b → pure (a + b , none))