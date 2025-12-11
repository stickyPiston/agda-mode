
-- Data type of HTML documents
-- Minimal contents, for internal use by renderer

module Iepje.Internal.Doc.Core (event : Set) where

import      Iepje.Internal.JS.WebAPIs.DOM as DOM
open import Iepje.Internal.JS.Language.IO using (IO)

open import Agda.Builtin.String
open import Agda.Builtin.Sigma

data Doc : Set where
  tag : String → Doc → Doc
  text : String → Doc
  attr : String → String → Doc -- applies to the *parent* element
  style : String → String → Doc -- applies to the *parent* element
  onIO : (js-event-name : String) -- applies to the *parent* element
    → (DOM.Event-of js-event-name .fst → IO event)
    → Doc
  doc-onIO : (js-event-name : String) -- applies to the root document
    → (DOM.Event-of js-event-name .fst → IO event)
    → Doc
  append : Doc → Doc → Doc
  empty : Doc
