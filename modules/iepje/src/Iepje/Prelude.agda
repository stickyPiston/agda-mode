
-- User-facing API of Iepje

module Iepje.Prelude where

open import Iepje.Internal.Gloss            public
open import Iepje.Internal.Doc.Core         public
open import Iepje.Internal.Doc.Combinators  public

open import Iepje.Internal.JS.Language.IO
  using (IO)
  public

open import Iepje.Internal.Utils
  using
    (case_of_ ; if_then_else_
    ;_$_ ; _&_  ; _∘_
    ; map ; for ; for' ; length
    ; not ; _&&_ ; _||_
    ; enumerate ; min ; max ; _/_ ; _%_
    ; _++_
    )
  public

open import Agda.Builtin.Unit   public
open import Agda.Builtin.Bool   public
open import Agda.Builtin.String public
open import Agda.Builtin.Nat    public
open import Agda.Builtin.Int    public
open import Agda.Builtin.Float  public
open import Agda.Builtin.Maybe  public
open import Agda.Builtin.List   public
open import Agda.Builtin.Sigma  public
