
-- Data structure tracking the state of browser DOM

module Iepje.Internal.Renderer.vDOM where

import      Iepje.Internal.JS.WebAPIs.DOM as DOM

open import Agda.Builtin.String

data vDOM : Set where
  tag   : DOM.HTMLElement → String → vDOM → vDOM
  text  : DOM.Text        → String → vDOM
  attr  : String → String → vDOM
  style : String → String → vDOM
  onIO     : (n : String) → DOM.event-listener n → vDOM
  doc-onIO : (n : String) → DOM.event-listener n → vDOM
  append : vDOM → vDOM → vDOM
  empty : vDOM
