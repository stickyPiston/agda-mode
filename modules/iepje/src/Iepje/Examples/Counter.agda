
-- A button that counts up when pressed

-- Demonstrates basic input

module Iepje.Examples.Counter where

open import Iepje.Prelude

counter : IO ⊤
counter = interact "#counter-app"
  0
  (λ n → button 1 $ text $ "Clicked " ++ (primShowNat n) ++ " times")
  _+_
