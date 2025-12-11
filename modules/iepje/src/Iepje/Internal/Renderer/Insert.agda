
-- Function to render new vDOM into the DOM

module Iepje.Internal.Renderer.Insert where

open import Agda.Builtin.Unit

open import Iepje.Internal.Doc.Core ⊤
open import Iepje.Internal.Renderer.vDOM
open import Iepje.Internal.Renderer.Cursor
open import Iepje.Internal.JS.Language.IO
open import Iepje.Internal.Utils

import      Iepje.Internal.JS.WebAPIs.DOM as DOM
open import Iepje.Internal.JS.Language.SubTyping

open import Agda.Builtin.String
open import Agda.Builtin.Sigma

open Cursor

private
  listen : DOM.EventTarget → (n : String) → (DOM.Event-of n .fst → IO ⊤) → IO (DOM.event-listener n)
  listen t n k = do
    l ← DOM.mk-event-listener k
    DOM.addEventListener t n l
    pure l

-- Postcondition: cursor moved after the inserted nodes
insert : Doc → Cursor → IO vDOM
insert (text  t) c = do e ← DOM.createTextNode (c .doc) t; insert-after (up e) c; text e t <$ pure tt
insert (tag t d) c = do e ← DOM.createElement  (c .doc) t; insert-after (up e) c; tag  e t <$> (insert d =<< init e)
insert (onIO     n k) c =     onIO n <$> listen (up (c .parent)) n k
insert (doc-onIO n k) c = doc-onIO n <$> listen (up (c    .doc)) n k
insert (attr  k v) _ = attr  k v <$ pure tt -- Hack: ignore attrs, always reapply in future pass
insert (style k v) _ = style k v <$ pure tt -- Hack: ignore style, always reapply in future pass
insert empty       _ = empty     <$ pure tt
insert (append d₀ d₁) c = append <$> insert d₀ c <*> insert d₁ c
