
-- Function to update modified DOM in place

module Iepje.Internal.Renderer.Darn where

open import Agda.Builtin.Unit

open import Iepje.Internal.Doc.Core ⊤

open import Iepje.Internal.Renderer.Insert
open import Iepje.Internal.Renderer.Delete

open import Iepje.Internal.Renderer.vDOM
open import Iepje.Internal.Renderer.Cursor

open import Iepje.Internal.Utils
open import Iepje.Internal.JS.Language.IO
open import Iepje.Internal.JS.Language.SubTyping
open import Iepje.Internal.JS.WebAPIs.DOM

open import Agda.Builtin.String
open import Agda.Builtin.Bool

private
  _==_ : String → String → Bool
  _==_ = primStringEquality

open Cursor

-- Precondition: cursor at beginning of rendered vDOM
darn : vDOM → Doc → Cursor → IO vDOM
darn d₀ d₁ c
  using redo ← do delete d₀ c ; insert d₁ c
  using when ← if_then_else redo
  with d₀ | d₁
-- These cases may contain focus to preserve
... | text e t₀    | text t₁    = when (t₀ == t₁) do text e t₀                             <$ curse (up e) c
... | tag  e t₀ d₀ | tag  t₁ d₁ = when (t₀ == t₁) do tag  e t₀ <$> (darn d₀ d₁ =<< init e) <* curse (up e) c
... | append l₀ r₀ | append l₁ r₁ = append <$> darn l₀ l₁ c <*> darn r₀ r₁ c
-- Anything else? Naively delete & re-insert.
... | _ | _ = redo
