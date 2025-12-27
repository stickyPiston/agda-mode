module TEA.Capability where

open import Iepje.Internal.JS.Language.IO
open import TEA.System
open import Prelude.Sigma
open import Prelude.Maybe
open import Agda.Builtin.Unit

postulate Disposable : Set

record Capability (msg : Set) : Set₁ where field
    requirement-type : Set
    new-requirement : System → IO requirement-type

    provided-type : Maybe (Σ[ provided-type ∈ Set ] (requirement-type → provided-type))

    register : System → requirement-type → (msg → IO ⊤) → IO Disposable