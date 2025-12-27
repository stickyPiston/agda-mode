module Prelude.Maybe where

open import Agda.Primitive using (Level)

data Maybe {ℓ : Level} (A : Set ℓ) : Set ℓ where
    nothing : Maybe A
    just    : A → Maybe A

{-# COMPILE JS Maybe   = ((x, v) => x === undefined ? v["nothing"]() : v["just"](x)) #-}
{-# COMPILE JS nothing = undefined #-}
{-# COMPILE JS just    = x => x #-}

fmap : ∀ {ℓ₁ ℓ₂} {A : Set ℓ₁} {B : Set ℓ₂} → (A → B) → Maybe A → Maybe B
fmap f nothing = nothing
fmap f (just x) = just (f x)

_<$>_ : ∀ {ℓ₁ ℓ₂} {A : Set ℓ₁} {B : Set ℓ₂} → (A → B) → Maybe A → Maybe B
_<$>_ = fmap