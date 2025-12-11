-- 'Cursor' type used in various DOM traverals

module Iepje.Internal.Renderer.Cursor where

import      Iepje.Internal.JS.WebAPIs.DOM as DOM
open import Iepje.Internal.JS.Language.IO
open import Iepje.Internal.JS.Language.Union
open import Iepje.Internal.JS.Language.PrimitiveTypes using (null)
open import Iepje.Internal.JS.Language.MutableReferences as Ref
open import Iepje.Internal.JS.Language.SubTyping
open import Iepje.Internal.JS.Language.FromUnion

open import Iepje.Internal.Utils

open import Agda.Builtin.Unit
open import Agda.Builtin.Maybe

-- A cursor points to a particular position in browser DOM
record Cursor : Set where
  constructor cursor
  field
    doc : DOM.Document           -- DOM Document containing the cursor
    parent : DOM.HTMLElement     -- DOM Element above the cursor
    left : Ref (Maybe DOM.Node)  -- DOM Node to the left of the cursor
  -- Invariant: the contents of `left` is a child of `parent` in the DOM
  -- Invariant: the contents of `left` & `parent` are contained in `doc`
open Cursor

-- Get the DOM Node after the cursor
right : Cursor → IO (DOM.Node ∪ null)
right c = Ref.get (c .left) >>= λ where
  (just l) → DOM.get-nextSibling (up l)
  nothing → DOM.get-firstChild (up (c .parent))

-- Make a new cursor pointing just before the first child of a DOM Element
init : DOM.HTMLElement → IO Cursor
init el = cursor <$> DOM.get-ownerDocument (up el) <*> pure el <*> Ref.new nothing

-- Move the cursor after a DOM Node
-- Precondition: node under cursor's parent in DOM
curse : DOM.Node → Cursor → IO ⊤
curse n c = set (c .left) (just n)

-- Insert a node after the cursor, and move the cursor after it
insert-after : DOM.Node → Cursor → IO ⊤
insert-after n c = do
  DOM.insertBefore (up (c .parent)) n =<< right c
  curse n c
