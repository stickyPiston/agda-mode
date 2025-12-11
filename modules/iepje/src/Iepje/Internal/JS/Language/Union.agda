
-- Library for un-typed unions

module Iepje.Internal.JS.Language.Union where

-- A value of type A ∪ B may have type A or type B at JS-run-time
postulate _∪_ : Set → Set → Set

infixr 20 _∪_

-- Values can be placed inside a union type
postulate inj₁ : ∀{A B} → A → A ∪ B
{-# COMPILE JS inj₁ = _ => _ => a => a #-}

postulate inj₂ : ∀{A B} → B → A ∪ B
{-# COMPILE JS inj₂ = _ => _ => b => b #-}
