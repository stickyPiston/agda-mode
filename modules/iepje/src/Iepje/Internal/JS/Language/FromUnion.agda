
-- Functions for inspecting unions

module Iepje.Internal.JS.Language.FromUnion where

open import Iepje.Internal.JS.Language.Union
open import Iepje.Internal.JS.Language.PrimitiveTypes using (null)

open import Agda.Builtin.Maybe

-- Binding to the `=== null` test
postulate from-∪-null : ∀{A} → A ∪ null → Maybe A
{-# COMPILE JS from-∪-null = _ => an =>
  (an === null)
    ? z_jAgda_Agda_Builtin_Maybe["Maybe"]["nothing"]
    : z_jAgda_Agda_Builtin_Maybe["Maybe"]["just"](an)
#-}
