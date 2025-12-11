
module Iepje.Examples.Dragger where

open import Iepje.Prelude

grid-box : ∀{e} → Nat → Nat → Nat → Nat → Doc e
grid-box x₀ x₁ y₀ y₁ = do
  style "grid-column-start" $ primShowNat x₀
  style "grid-column-end"   $ primShowNat x₁
  style "grid-row-start"    $ primShowNat y₀
  style "grid-row-end"      $ primShowNat y₁

insertion-list : ∀ {e} → Bool → Bool → (Nat → e) → List (Doc e) → (Nat → Doc e) → Doc e
insertion-list enabled debug hover docs divider = do
    style "display" "grid"
    -- List items
    concatDocs $ for' docs λ i d → do
      div do d; grid-box 0 1 (3 * i + 2) (3 * i + 4)  -- 2 cells
    -- Hitboxes & dividers
    let l = length docs
    concatDocs $ for (enumerate (1 + l)) λ i → do
      div do  -- Divider, covers 1 cell between items
        grid-box 0 1 (3 * i + 1) (3 * i + 2)
        divider i
      when enabled $ div do -- Hitbox, covering divider & neighbours
        grid-box 0 1 (max 1 (3 * i + 0)) (min (3 * l + 2) (3 * i + 3))
        style "width" "0"
        div do
          style "margin-left" "-100vw"  -- hack to fill screen horizontally, overflows in x
          -- CSS anchor positioning might avoid overflow,
          -- but is still experimantal and unsupported by firefox:
          -- https://developer.mozilla.org/en-US/docs/Web/CSS/position-anchor#browser_compatibility
          style "height" "100%"
          style "width" "200vw"
          on "mouseover" λ _ → hover i
          style "user-select" "none"
          style "z-index" "20"
          style "position" "relative"
          when debug do
            style "opacity" "20%"
            style "background" "green"
            style "box-shadow" "inset 0 0 0 1px red"

data Event : Set where
  drag-over : Nat → Event
  cancel-drag : Event
  lift-marker : Event
  drop-marker : Event
  toggle-debug : Event

record Model : Set where
  field items : List String
  field marker-pos : Nat
  field caret-pos : Maybe Nat
  field debug : Bool
open Model

dragging : Model → Bool
dragging m = case m .caret-pos of λ where
  (just _) → true
  nothing → false

view : Model → Doc Event
view m = col do
  doc-on "mouseup" λ _ → drop-marker
  doc-on "blur" λ _ → cancel-drag

  let mi = m .marker-pos
  let ci = case m .caret-pos of λ where (just ci) → ci; nothing → mi
  insertion-list (dragging m) (debug m) drag-over
    (for' (m .items) λ i n → do -- Cells
      style "padding" "0.1cm"
      style "border-radius" "0.5cm"
      style "border" "0.05cm solid black"
      style "display" "inline-grid"
      style "grid-template-columns" "1ch auto 1ch"
      when (dragging m) do style "user-select" "none"
      when (i < mi) do style "opacity" "50%"
      when (ci < 1 + i && i < mi || mi < 1 + i && i < ci) do span do
        style "grid-column" "1"
        style "justify-self" "right"
        text "⋆"
      span do
        style "grid-column" "2"
        text n
      do -- Hidden toggle for debug mode
        attr "tabindex" "-1"  -- listen for keypresses
        on "keydown" λ _ → toggle-debug
    )
    (λ i → do  -- Markers
      when (i == mi) do  -- Main marker
        style "height" "0.4cm"
        style "background" (if dragging m then "lightgrey" else "darkgrey")
        on "mousedown" λ _ → lift-marker
      when (i == ci) do -- Insertion caret
        style "box-shadow" "inset 0 0 0 0.1cm darkgrey"
      style "min-height" "0.1cm"
      style "margin" "0.05cm"
      style "user-select" "none"
    )
  when (debug m) do
    style "box-shadow" "inset 0 0 0 1px red"
    span do
      text "Debug mode (click to exit)"
      on "click" λ _ → toggle-debug
      style "font-size" "80%"; style "font-style" "italic"
      style "width" "0"; style "min-width" "100%" -- https://stackoverflow.com/a/56722703


update : Event → Model → Model
update (drag-over x)
  r@(record  {caret-pos = just _})
  = record r {caret-pos = just x}
update drop-marker
  r@(record  {caret-pos = just x})
  = record r {marker-pos = x; caret-pos = nothing}
update cancel-drag   r = record r {caret-pos = nothing} --cancel drag
update lift-marker r = record r {caret-pos = just (r .marker-pos)}
update toggle-debug r = record r {debug = not (r .debug)}
update _ r = r

m0 : Model
m0 .items = "apricot" ∷ "bannana" ∷ "cherry" ∷ "grapefruit" ∷ "pear" ∷ "quince" ∷ []
m0 .marker-pos = 2
m0 .caret-pos = nothing
m0 .debug = false

dragger : IO ⊤
dragger = interact "#dragger-app" m0 view update

