
-- Library for mutable references

-- * Backed by JS variables
-- * Accessible only in IO

module Iepje.Internal.JS.Language.MutableReferences where

open import Iepje.Internal.JS.Language.IO
open import Iepje.Internal.Utils

open import Agda.Builtin.Unit

-- Type of mutable references
postulate Ref : Set → Set

-- Implemented in JS as an object with a single, mutable field, `val`

-- Create a new mutable reference
postulate new : ∀{A} → A → IO (Ref A)
{-# COMPILE JS new = _ => a => krefa => krefa ({val : a}) #-}

-- Update the value stored in a mutable reference & return the new value
postulate modify : ∀{A} → Ref A → (A → A) → IO A
{-# COMPILE JS modify = _ => refa => f => kt => kt(refa.val = f(refa.val)) #-}

-- Derived helper functions
set : ∀{A} → Ref A → A → IO ⊤
set r a = do
  modify r λ _ → a
  pure tt

get : ∀{A} → Ref A → IO A
get r = modify r λ x → x
