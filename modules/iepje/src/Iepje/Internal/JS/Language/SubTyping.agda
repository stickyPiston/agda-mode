
-- Agda interface to JavaScript sub-classing

module Iepje.Internal.JS.Language.SubTyping where

-- (A extends B) iff, after compilation to JavaScript,
-- A's direct super-class is B
postulate _extends_ : Set → Set → Set

-- When (A extends B), a value of A may be used wherever a B is expected
-- (aka the "Liskov substitution principle")
postulate upcast : ∀{A B : Set} → {{A extends B}} → A → B
{-# COMPILE JS upcast = _ => _ => _ => a => a #-}

-- Helper definition for 'up'
data _extends*_ (base : Set) : Set → Set₁ where instance
  extends-refl : base extends* base
  extends-cons : ∀{parent ancestor}
    → {{base   extends  parent  }}
    → {{parent extends* ancestor}}
    → base extends* ancestor
{-# OVERLAPS extends-refl extends-cons #-}

-- Convenience function: up-cast several times
up : ∀{A B : Set} → {{A extends* B}} → A → B
up {{extends-refl}} a = a
up {{extends-cons {{p}} {{p*}}}} a = up {{p*}} (upcast {{p}} a)

-- Misc. helper for getting a value from instance search
it : ∀{ℓ}{A : Set ℓ} → {{A}} → A
it {{a}} = a
