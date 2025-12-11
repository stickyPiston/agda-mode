module Communication where

open import Iepje.Internal.JS.Language.PrimitiveTypes
open import Iepje.Internal.Utils

open import Agda.Builtin.List

data Maybe (A : Set) : Set where
    nothing : Maybe A
    just : A → Maybe A

{-# COMPILE JS Maybe = (x, v) =>
    v === undefined
        ? x["nothing"]()
        : x["just"](v) #-}
{-# COMPILE JS nothing = undefined #-}
{-# COMPILE JS just = x => x #-}

data JSON : Set where
    j-null : JSON
    j-string : string → JSON
    j-bool : boolean → JSON
    j-number : number → JSON
    j-array : List JSON → JSON
    j-object : List (string × JSON) → JSON

data Msg : Set where
    a b : Msg