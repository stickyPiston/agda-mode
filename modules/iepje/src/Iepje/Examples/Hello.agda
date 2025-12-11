
-- Minimal example

module Iepje.Examples.Hello where

open import Iepje.Prelude

hello : IO ⊤
hello = display "#hello-app"
  $ text "Hello Iepje!"
