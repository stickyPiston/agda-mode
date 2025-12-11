
-- Function to re-apply styles from a vDOM

module Iepje.Internal.Renderer.Style where

open import Iepje.Internal.Renderer.vDOM

import      Iepje.Internal.JS.WebAPIs.DOM as DOM
import      Iepje.Internal.JS.WebAPIs.CSSOM as CSSOM
open import Iepje.Internal.JS.Language.SubTyping
open import Iepje.Internal.JS.Language.IO
open import Iepje.Internal.JS.Language.MutableReferences as Ref
open import Iepje.Internal.Renderer.Cursor
open import Iepje.Internal.Utils

open import Agda.Builtin.Unit
open import Agda.Builtin.Maybe

open Cursor

-- Pre-condition: cursor has correct parent
-- Postcondition: cursor unmoved
re-style : vDOM → Cursor → IO ⊤
re-style (attr  k v)    c = do DOM.setAttribute (up (c .parent)) k v; pure tt
re-style (style k v)    c = do css ← DOM.get-style (up (c .parent)); CSSOM.setProperty css k v; pure tt
re-style (tag e _ d)    c = do re-style d =<< init e
re-style (append d₀ d₁) c = do re-style d₀ c ; re-style d₁ c
re-style (text _ _)     c = pure tt
re-style (onIO     _ _) c = pure tt
re-style (doc-onIO _ _) c = pure tt
re-style empty          c = pure tt
