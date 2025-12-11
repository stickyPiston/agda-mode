
-- Agda bindings to the JavaScript CSSOM API,
-- https://developer.mozilla.org/en-US/docs/Web/API/CSS_Object_Model

module Iepje.Internal.JS.WebAPIs.CSSOM where

open import Iepje.Internal.JS.Language.IO
open import Iepje.Internal.JS.Language.PrimitiveTypes

open import Agda.Builtin.String

----------------------------------------------------------------
-- Type-level bindings
----------------------------------------------------------------

postulate
  CSSStyleDeclaration : Set -- super-class unclear

----------------------------------------------------------------
-- Value level bindings with simple types
----------------------------------------------------------------

-- CSSStyleDeclaration

postulate setProperty : CSSStyleDeclaration → String → String → IO undefined
{-# COMPILE JS setProperty = sd => k => v => kt => kt(sd.setProperty(k,v)) #-}

postulate removeProperty : CSSStyleDeclaration → String → IO string
{-# COMPILE JS removeProperty = sd => k => kv => kv(sd.removeProperty(k)) #-}
