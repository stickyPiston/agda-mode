module Prelude.Maybe where

open import Agda.Primitive using (Level)

data Maybe {ℓ : Level} (A : Set ℓ) : Set ℓ where
    nothing : Maybe A
    just    : A → Maybe A

{-# COMPILE JS Maybe   = ((x, v) => x === undefined ? v["nothing"]() : v["just"](x)) #-}
{-# COMPILE JS nothing = undefined #-}
{-# COMPILE JS just    = x => x #-}