module Iepje.Internal.Effect where

open import Agda.Builtin.List
open import Agda.Builtin.Unit
open import Agda.Builtin.Nat

open import Iepje.Internal.JS.Language.IO

private variable msg : Set

record Effect msg : Set where
    field actions : List ((msg → IO ⊤) → IO ⊤)

from : ((dispatch : msg → IO ⊤) → IO ⊤) → Effect msg
from effect = record { actions = effect ∷ [] }

none : ∀ {msg} → Effect msg
none = record { actions = [] }