
-- Gloss-style entry points

-- Based on
-- https://hackage.haskell.org/package/gloss-1.13.2.2/docs/Graphics-Gloss.html

module Iepje.Internal.Gloss where

open import Iepje.Internal.Doc.Core

import      Iepje.Internal.Runtime as Runtime
open import Iepje.Internal.Effect

open import Iepje.Internal.Utils
import      Iepje.Internal.JS.WebAPIs.DOM as DOM

open import Iepje.Internal.JS.Language.IO
open import Iepje.Internal.JS.Language.SubTyping
open import Iepje.Internal.JS.Language.FromUnion

import Iepje.Internal.JS.Language.GlobalObjects
  as Date using (now)
import Iepje.Internal.JS.Language.MutableReferences as Ref

open import Agda.Builtin.Unit
open import Agda.Builtin.String
open import Agda.Builtin.List
open import Agda.Builtin.Nat
open import Agda.Builtin.Float
open import Agda.Builtin.Maybe
open import Agda.Builtin.Sigma

private variable
  world model : Set
  event : Set

CssSelector = String

-- Impure version of play
playIO
  : CssSelector
  → Nat
  → (world × Effect event)
  → (world → IO (Doc event))
  → (event → world → IO (world × Effect event))
  → (Float → world → IO world)
  → IO ⊤
playIO sel freq (m0 , e0) view update step = do
  rs ← Runtime.create-Store m0
  d ← DOM.document
  w ← DOM.get-defaultView d
  -- Add the view
  just p ← from-∪-null <$> DOM.querySelector d sel
    where nothing → pure tt
  div ← DOM.createElement d "div"
  DOM.appendChild (up p) (up div)
  Runtime.addView rs view update (up div) e0
  -- Set up periodic updates, if the user requested them
  -- case freq of λ where
  --   zero         → pure tt
  --   freq@(suc _) → do
  --     rt ← Ref.new =<< Date.now
  --     let periodInMillis = primNatToFloat $ 1000 / freq
  --     DOM.setInterval w
  --       (λ _ → Runtime.apply-update-and-schedule-refresh rs λ m → do
  --           t-prev ← Ref.get rt
  --           t-now ← Date.now
  --           Ref.set rt t-now
  --           let _-_ = primFloatMinus; _/_ = primFloatDiv
  --           let elapsedSecs = (t-now - t-prev) / 1000.0
  --           step elapsedSecs m
  --       )
  --       periodInMillis
  --     pure tt

-- Impure version of interact
interactIO
  : CssSelector
  → (model × Effect event)
  → (view : model → IO (Doc event))
  → (update : event → model → IO (model × Effect event))
  → IO ⊤
interactIO sel m0 view update = do
  playIO sel 0 m0 view update λ _ → pure

-- Play a game in a DOM node
play
  : CssSelector -- Element to display under
  → Nat         -- Number of simulation steps to take for each second of real time
  → world       -- The initial world
  → (world → Doc event)     -- A function to convert the world to a document
  → (event → world → world) -- A function to handle input events
  → (Float → world → world) -- A function to step the world one iteration. It is passed the period of time (in seconds) needing to be advanced
  → IO ⊤
play sel freq m0 view update step = do
  playIO sel freq (m0 , none)
    (λ   w → pure $ view     w)
    (λ e w → pure (update e w , none))
    (λ t w → pure $ step t   w)

-- Run an interactive program in a DOM node
interact
  : CssSelector -- Element to display under
  → model       -- The initial model
  → (model → Doc event)     -- A function to convert the model to a document
  → (event → model → model) -- A function to handle input events
  → IO ⊤
interact sel m0 view update = do
  interactIO sel (m0 , none)
    (λ   m → pure $ view     m)
    (λ e m → pure (update e m , none))

-- Display the given document in a DOM node
display
  : CssSelector -- Element to display under
  → Doc ⊤       -- Document to display
  → IO ⊤
display sel doc = interact sel tt (const doc) const
