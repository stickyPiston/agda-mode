
-- Video game from 1972

-- Demonstrates fancy styling, keyboard bindings, and focus retention

module Iepje.Examples.Pong where

open import Iepje.Prelude
  hiding (_+_;_-_; _*_; _/_; _<_)

-- Units:
--  time: ticks
--  distance: CSS pixels
--  speed: CSS pixels/tick

-- x/y positions are measured from the center of the field
-- to the center of the object

update-frequency = 100 -- ticks per second
_<_ = primFloatLess ; infix 19 _<_
_+_ = primFloatPlus ; infixl 21 _+_
_-_ = primFloatMinus; infixl 21 _-_
_/_ = primFloatDiv  ; infixl 22 _/_
_*_ = primFloatTimes; infixl 22 _*_
-_ = primFloatNegate; infixr 23 -_

record Paddle : Set where
  field y y' : Float
  field goals-conceded : Nat
open Paddle
paddle-height = 90.0
paddle-width  = 5.0
paddle-offset = 10.0

record Ball : Set where
  field x y x' y' : Float
open Ball
ball-diameter = 10.0

record Field : Set where
  field left right : Paddle
  field ball : Ball
open Field
field-height = 300.0
field-width = 400.0

initial-Field : Field
initial-Field = λ where
  .left  → record {y = 0.0; y' = 0.0; goals-conceded = 0}
  .right → record {y = 0.0; y' = 0.0; goals-conceded = 0}
  .ball  → record {x = 0.0; y = 0.0; x' = 2.0; y' = 2.0}

shift-Paddle : Paddle → Paddle
shift-Paddle p = record p {y = p .y + p .y'}

shift-Ball : Ball → Ball
shift-Ball b = record b {x = b .x + b .x'; y = b .y + b. y'}

bounce-y : Ball → Ball
bounce-y b
  = let
    y = b .y
    y' = b .y'
    ymax = field-height / 2.0
    ymin = - ymax
  in   if ymax < y then record b {y = 2.0 * ymax - y; y' = - y'}
  else if y < ymin then record b {y = 2.0 * ymin - y; y' = - y'}
  else b

bounce-x : Ball → Ball
bounce-x b
  = let
    x = b .x
    x' = b .x'
    xmax =   field-width / 2.0
    xmin = - field-width / 2.0
  in   if xmax < x then record b {x = 2.0 * xmax - x; x' = - x'}
  else if x < xmin then record b {x = 2.0 * xmin - x; x' = - x'}
  else b

check-goal-if : Ball → (Float → Bool) → Paddle → Paddle
check-goal-if b cond p =
  if cond (b .x) && (b .y < ymin || ymax < b .y)
  then record p {goals-conceded = suc (p .goals-conceded)}
  else p
  where
    ymin = p .y - paddle-height / 2.0
    ymax = p .y + paddle-height / 2.0

step-field : Field → Field
step-field f = λ where
  .left  → check-goal-if b (_< xmin) ∘ shift-Paddle $ f .left
  .right → check-goal-if b (xmax <_) ∘ shift-Paddle $ f .right
  .ball  → bounce-x ∘ bounce-y $ b
    where
      b = shift-Ball $ ball f
      xmin = - field-width / 2.0
      xmax =   field-width / 2.0

data Paddle-Cmd : Set where
  ↑| ↓| -| |- |↑ |↓ other : Paddle-Cmd

psf = primShowFloat

module _ where

  view-Paddle : ∀{e} → Paddle → String → Doc e
  view-Paddle paddle left|right = div do
    style "position" "relative"
    style "background" "none"
    style "height" "100%"
    style "width" $ psf paddle-width ++ "px"
    style "z-index" "10"
    div do
      style "position" "absolute"
      style "top" "0.5em"
      style left|right "2ch"
      text $ primShowNat (paddle .goals-conceded)
    div do
      style "height" $ psf paddle-height ++ "px"
      style "width" "100%"
      style "background" "black"
      style "position" "absolute"
      style "top" $ "calc(50% + "++ psf (paddle .y) ++"px)"
      style "transform" "translate(0, -50%)" -- set y-position based on center, not top

  view-Ball : ∀{e} → Ball → Doc e
  view-Ball ball = div do
    style "background" $ "red"
    style "width" $ ""++ psf ball-diameter ++"px"
    style "height" $ ""++ psf ball-diameter ++"px"
    style "border-radius" $ "50%"
    style "position" $ "absolute"
    style "left" $ "calc(50% + "++ psf (ball .x) ++"px)"
    style "top" $ "calc(50% + "++ psf (ball .y) ++"px)"
    style "transform" $ "translate(-50%, -50%)"  -- set x/y-position based on center, not left/top
    -- text "x=" ; text (psf (ball .x)) -- debug
    -- br
    -- text "y=" ; text (psf (ball .y)) -- debug

  view-Field : Field → Doc Paddle-Cmd
  view-Field f = row do
    style "height" $ ""++ psf field-height ++"px"
    attr "tabindex" "0"
    view-Paddle (f .left) "left"
    div do
      style "width" $ ""++ psf field-width ++"px"
      style "position" $ "relative"
      style "background" $ "lightgrey"
      view-Ball (f .ball)
    view-Paddle (f .right) "right"
    on-key-down λ where
      "d" → ↑|  ;  "j" → |↑
      "f" → ↓|  ;  "k" → |↓
      _ → other
    on-key-up λ where
      "d" → -|  ;  "j" → |-
      "f" → -|  ;  "k" → |-
      _ → other

set-y' : Float → Paddle → Paddle
set-y' y' p = record p {y' = y'}

pong : IO ⊤
pong = play "#pong-game"
  update-frequency
  initial-Field
  view-Field
  (λ where
    ↑| f → record f {left  = set-y' -5.0 (f .left )}
    -| f → record f {left  = set-y'  0.0 (f .left )}
    ↓| f → record f {left  = set-y'  5.0 (f .left )}
    |↑ f → record f {right = set-y' -5.0 (f .right)}
    |- f → record f {right = set-y'  0.0 (f .right)}
    |↓ f → record f {right = set-y'  5.0 (f .right)}
    other f → f)
  (λ _ → step-field)
 