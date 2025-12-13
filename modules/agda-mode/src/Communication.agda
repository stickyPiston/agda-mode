module Communication where

open import Iepje.Internal.JS.Language.PrimitiveTypes
open import Iepje.Internal.Utils hiding (_×_)

open import Agda.Builtin.List
open import Agda.Builtin.Nat
open import Agda.Builtin.Maybe

data _×_ (A B : Set) : Set where
    _,_ : A → B → A × B

{-# COMPILE JS _×_ = ([a, b], v) => v["_,_"](a, b) #-}
{-# COMPILE JS _,_ = a => b => [a, b] #-}

data JSON : Set where
    j-null : JSON
    j-string : string → JSON
    j-bool : boolean → JSON
    j-number : number → JSON
    j-array : List JSON → JSON
    j-object : List (string × JSON) → JSON

{-# COMPILE JS JSON = ((x, v) =>
      x === null             ? v["j-null"]()
    : typeof x === "string"  ? v["j-string"](x)
    : typeof x === "boolean" ? v["j-bool"](x)
    : typeof x === "number"  ? v["j-number"](x)
    : Array.isArray(x)       ? v["j-array"](x) /* uhhhhh */
                             : v["j-object"](Object.entries(x)))
    #-}
{-# COMPILE JS j-null = null #-}
{-# COMPILE JS j-string = s => String(s) #-}
{-# COMPILE JS j-bool = b => Boolean(b) #-}
{-# COMPILE JS j-number = n => Number(n) #-}
{-# COMPILE JS j-array = l => [...l] #-}
{-# COMPILE JS j-object = kvs => kvs.reduce((acc, [k, v]) => ({ ...acc, [k]: v }), {}) #-}

data Msg : Set where
    a b : Msg

encode : Msg → JSON
encode a = j-array (j-string "a" ∷ [])
encode b = j-array (j-string "b" ∷ [])

decode : JSON → Maybe Msg
decode (j-array (j-string "a" ∷ [])) = just a
decode (j-array (j-string "b" ∷ [])) = just b
decode _ = nothing