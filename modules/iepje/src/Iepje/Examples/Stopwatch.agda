
-- A stopwatch with the usual buttons

-- Demonstrates fancy styling, subscription to time, and focus retention

module Iepje.Examples.Stopwatch where

open import Iepje.Prelude

Milliseconds = Nat

record State : Set where field
  count     : Milliseconds
  lap-count : Maybe Milliseconds
  running : Bool
open State

data Button : Set where
  pause reset lap : Button

view : State → Doc Button
view s = col do -- Stopwatch body
  style "border-radius" "1ch"
  style "background" "Black"
  style "gap" "0.5ch"; style "padding" "0.5ch"
  row do  -- Stopwatch screen
    style "border-radius" "1ch"
    style "background" "DarkSeaGreen"
    style "font-family" "monospace"
    style "white-space" "pre"
    style "justify-content" "space-between"
    style "padding" "0.5ch"
    div do
      text $ if lapped then "LAP" else "   "
    div do
      style "font-size" "200%"
      style "justify-content" "flex-end"
      style "align-items" "baseline"
      span do text $ psn $ minutes / 10 % 6
      span do text $ psn $ minutes % 10
      span do text $ if paused || centis % 100 < 50 then ":" else " "
      span do text $ psn $ seconds / 10 % 6
      span do text $ psn $ seconds % 10
      span do text $ psn $ centis  / 10 % 10; style "font-size" "80%"
      span do text $ psn $ centis  % 10     ; style "font-size" "80%"
  row do  -- Stopwatch buttons
    style "justify-content" "space-evenly"
    button pause $ text "Pause"
    button reset $ text "Reset"
    button lap   $ text "Lap"
  where
    psn = primShowNat
    paused = not $ s .running
    lapped = case s .lap-count of λ where (just -) → true; nothing  → false
    millis = case s .lap-count of λ where (just x) → x; nothing  → s .count
    centis  = millis / 10
    seconds = centis / 100
    minutes = seconds / 60

-- Time between model updates (in miliseconds)
time-granularity = 25

stopwatch : IO ⊤
stopwatch = play "#stopwatch-app"
  (1000 / time-granularity)
  (λ where
    .count → 0
    .lap-count → nothing
    .running → true
  )
  view
  (λ where
    pause s → record s {running   = not (s .running)}
    reset s → record s {count     = 0}
    lap   s → record s {lap-count =
      case s .lap-count of λ where
        (just _) → nothing
        nothing  → just (s .count)
      }
  )
  λ extra-seconds s →
    if not (s .running) then s else
    let extra-millis = primFloatRound $ primFloatTimes extra-seconds 1000.0
    in record s {count = s .count +
      case extra-millis of λ where
        (just (pos x)) → x ; _ → 0
      }
