module Prelude.Maybe where

open import Agda.Primitive using (Level)
open import Prelude.List using (List ; map ; [] ; _∷_)
open import Agda.Builtin.String
open import Prelude.Sigma
open import Iepje.Internal.Utils using (if_then_else_)

private variable 
    ℓ₁ ℓ₂ : Level
    A : Set ℓ₁
    B : Set ℓ₂

data Maybe {ℓ} (A : Set ℓ) : Set ℓ where
    nothing : Maybe A
    just    : A → Maybe A

{-# COMPILE JS Maybe   = ((x, v) => x === undefined ? v["nothing"]() : v["just"](x)) #-}
{-# COMPILE JS nothing = undefined #-}
{-# COMPILE JS just    = x => x #-}

fmap : (A → B) → Maybe A → Maybe B
fmap f nothing = nothing
fmap f (just x) = just (f x)

_<$>_ : (A → B) → Maybe A → Maybe B
_<$>_ = fmap

_<*>_ : Maybe (A → B) → Maybe A → Maybe B
just f <*> just a = just (f a)
_ <*> _ = nothing

pure : A → Maybe A
pure = just

_>>=_ : Maybe A → (A → Maybe B) → Maybe B
nothing >>= f = nothing
just x >>= f = f x
infixl 10 _>>=_

justs : List (Maybe A) → List A
justs [] = []
justs (nothing ∷ as) = justs as
justs (just a ∷ as) = a ∷ justs as

map-maybe : (A → Maybe B) → List A → List B
map-maybe f as = justs (map f as)

traverse : (A → Maybe B) → List A → Maybe (List B)
traverse f [] = just []
traverse f (a ∷ as) = ⦇ f a ∷ traverse f as ⦈

lookup : String → List (String × A) → Maybe A
lookup name [] = nothing
lookup name ((k , v) ∷ kvs) = if primStringEquality name k then just v else lookup name kvs

_!?_ : List (String × A) → String  → Maybe A
m !? k = lookup k m