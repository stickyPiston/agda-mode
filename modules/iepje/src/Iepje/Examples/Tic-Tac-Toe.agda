
-- A Tic Tac Toe game written with Iepje

module Iepje.Examples.Tic-Tac-Toe where

open import Iepje.Prelude

data Cell : Set where
  [-] [o] [x] : Cell

next : Cell → Cell
next = λ where [-] → [-]; [x] → [o]; [o] → [x]

view-Cell  : ∀{e} → Cell → Doc e
view-Cell c = span do
  style "font-style" "bold"; style "font-family" "sans"
  case c of λ where
    [-] → do text " "
    [x] → do text "O"; style "color" "blue"
    [o] → do text "X"; style "color" "red"

data Pos : Set where
    ╔  ╤ ╗
     ╠ ╬ ╢
     ╚ ╧ ╝ : Pos

record Board : Set where
  constructor mk-Board; field
    ┌  ┬ ┐
     ├ ┼ ┤
     └ ┴ ┘ : Cell

get-Cell : Pos → Board → Cell
get-Cell = let open Board in λ where
  ╔ → ┌ ; ╤ → ┬ ; ╗ → ┐
  ╠ → ├ ; ╬ → ┼ ; ╢ → ┤
  ╚ → └ ; ╧ → ┴ ; ╝ → ┘

set-Cell : Board → Cell → Pos → Board
set-Cell b c = let open Board in λ where
  ╔ → record b {┌ = c} ; ╤ → record b {┬ = c} ; ╗ → record b {┐ = c}
  ╠ → record b {├ = c} ; ╬ → record b {┼ = c} ; ╢ → record b {┤ = c}
  ╚ → record b {└ = c} ; ╧ → record b {┴ = c} ; ╝ → record b {┘ = c}

record State : Set where
  field board : Board
  field to-move : Cell
open State

s0 : State
s0 .to-move = [x]
s0 .board = mk-Board
  [-] [-] [-]
  [-] [-] [-]
  [-] [-] [-]

winner : Board → Maybe Cell
winner = λ where
  (mk-Board [x]  _   _
            [x]  _   _
            [x]  _   _) → just [x]

  (mk-Board  _  [x]  _
             _  [x]  _
             _  [x]  _) → just [x]

  (mk-Board  _   _  [x]
             _   _  [x]
             _   _  [x]) → just [x]

  (mk-Board [x] [x] [x]
             _   _   _
             _   _   _ ) → just [x]

  (mk-Board  _   _   _
            [x] [x] [x]
             _   _   _ ) → just [x]

  (mk-Board  _   _   _
             _   _   _
            [x] [x] [x]) → just [x]

  (mk-Board [x]  _   _
             _  [x]  _
             _   _  [x]) → just [x]

  (mk-Board  _   _  [x]
             _  [x]  _
            [x]  _   _ ) → just [x]

  (mk-Board [o]  _   _
            [o]  _   _
            [o]  _   _ ) → just [o]

  (mk-Board  _  [o]  _
             _  [o]  _
             _  [o]  _ ) → just [o]

  (mk-Board  _   _  [o]
             _   _  [o]
             _   _  [o]) → just [o]

  (mk-Board [o] [o] [o]
             _   _   _
             _   _   _ ) → just [o]

  (mk-Board  _   _   _
            [o] [o] [o]
             _   _   _ ) → just [o]

  (mk-Board  _   _   _
             _   _   _
            [o] [o] [o]) → just [o]

  (mk-Board [o]  _   _
              _  [o]  _
              _   _  [o]) → just [o]

  (mk-Board  _   _  [o]
             _  [o]  _
            [o]  _   _ ) → just [o]

  (mk-Board [-]  _   _
             _   _   _
             _   _   _ ) → nothing
  (mk-Board  _  [-]  _
             _   _   _
             _   _   _ ) → nothing
  (mk-Board  _   _  [-]
             _   _   _
             _   _   _ ) → nothing
  (mk-Board  _   _   _
            [-]  _   _
             _   _   _ ) → nothing
  (mk-Board  _   _   _
             _  [-]  _
             _   _   _ ) → nothing
  (mk-Board  _   _   _
             _   _  [-]
             _   _   _ ) → nothing
  (mk-Board  _   _   _
             _   _   _
            [-]  _   _ ) → nothing
  (mk-Board  _   _   _
             _   _   _
             _  [-]  _ ) → nothing
  (mk-Board  _   _   _
             _   _   _
             _   _  [-]) → nothing
  (mk-Board  _   _   _
             _   _   _
             _   _   _ ) → just [-]

view : State → Doc Pos
view s = col do
  style "align-items" "center"
  table do
    tr do v ╔ ; v ╤ ; v ╗
    tr do v ╠ ; v ╬ ; v ╢
    tr do v ╚ ; v ╧ ; v ╝
  case winner (s .board) of λ where
    nothing    →    (span do view-Cell $        s .to-move; text " to play")
    (just [-]) → do (span do text "Game drew")                          ; hint
    (just x)   → do (span do view-Cell $ next $ s .to-move; text " won"); hint
  where
  v : Pos → Doc Pos
  v p = td ∘ button p $ do
    style "width" "2em"; style "height" "2em"
    style "background-color" "white"
    view-Cell (get-Cell p (s .board))
  hint = span do
    text "click any tile to reset"
    style "font-size" "80%"; style "font-style" "italic"
    style "width" "0"; style "min-width" "100%" -- https://stackoverflow.com/a/56722703

update : Pos → State → State
update p (s@record{board = b; to-move = c}) = case (winner b , get-Cell p b) of λ where
  (just _ , _) →  s0
  (_ , [o]) → s -- Player only allowed to click on blanks, ignore their click
  (_ , [x]) → s -- Player only allowed to click on blanks, ignore their click
  (_ , [-]) → record {board = set-Cell b c p; to-move = next c}

tic-tac-toe : IO ⊤
tic-tac-toe = interact "#ttt-game"
  s0 view update
