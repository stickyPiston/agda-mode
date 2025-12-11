
-- Imperative glue code impementing The Elm Architecture

module Iepje.Internal.Runtime where

import      Iepje.Internal.Renderer.vDOM    as vDOM
import      Iepje.Internal.Renderer.Cursor  as Cursor
import      Iepje.Internal.Renderer.Darn    as Renderer
import      Iepje.Internal.Renderer.vDOM    as Renderer
import      Iepje.Internal.Renderer.Style   as Renderer

open import Iepje.Internal.Effect

open import Iepje.Internal.Doc.Core as Html
import      Iepje.Internal.Doc.Combinators as Html

open import Iepje.Internal.Utils

import      Iepje.Internal.JS.WebAPIs.DOM as DOM
open import Iepje.Internal.JS.Language.IO
open import Iepje.Internal.JS.Language.MutableReferences
open import Iepje.Internal.JS.Language.SubTyping

open import Agda.Builtin.Unit
open import Agda.Builtin.String
open import Agda.Builtin.List
open import Agda.Builtin.Sigma

-- State needed to run a TEA application
record Runtime-Store (model : Set) : Set where
  constructor runtime-store
  -- The current state of the model
  field current-model : Ref model
  -- IO actions to run on each animation tick
  field on-animation-tick-hooks : Ref (List (IO ⊤))
open Runtime-Store

create-Store : ∀{model} → model → IO (Runtime-Store model)
create-Store {model} m0 = runtime-store <$> new m0 <*> new []

-- Run all the hooks in a Runtime-Store, immediately
run-hooks : ∀{model} → Runtime-Store model → IO ⊤
run-hooks rs = do
  get (rs .on-animation-tick-hooks) >>= sequenceA
  pure tt

-- Run all the hooks in a Runtime-Store, on the next browser animation frame
schedule-refresh : ∀{model} → Runtime-Store model → IO ⊤
schedule-refresh rs = do
  -- Currently, this function always schedules a re-render,
  -- even if there is already one pending for the next-frame.
  -- This could be avoided by checking a 'dirty bit' here.
  d ← DOM.document
  w ← DOM.get-defaultView d
  DOM.requestAnimationFrame w λ _ → run-hooks rs
  pure tt

-- Update the model, and schedule a refresh on the next browser animation frame
{-# TERMINATING #-}
apply-update-and-schedule-refresh : ∀{model event} → Runtime-Store model → event → (event → model → IO (model × Effect event)) → IO ⊤
apply-update-and-schedule-refresh rs e update = do
  m ← get $ rs .current-model
  m' , effect ← update e m
  forM_ (effect .actions) λ action →
    action λ e → apply-update-and-schedule-refresh rs e update
  set (rs .current-model) m'
  schedule-refresh rs
  where open Effect

-- Add a view to a Runtime-Store, under an existing Element
-- The view is re-rendered incrementally each update
addView
  : ∀{model event}
  → Runtime-Store model
  → (view : model → IO (Doc event))
  → (update : event → model → IO (model × Effect event))
  → DOM.HTMLElement
  → Effect event
  → IO ⊤
addView rs view update parent e0 = do
  -- Delete all children of the element, to ensure the vDOM matches
  DOM.replaceChildren (up parent) []
  -- Create a muable reference cell to track the state of this view
  rvd ← new vDOM.empty
  -- Create a callback which renders this view when called
  let renderThisView = do
    m ← get $ rs .current-model
    doc ← view m  -- Generate the (declarative) Doc
      -- Modify the Doc's callbacks to re-enter the runtime
      <&> Html.mapDocIO λ e →
        apply-update-and-schedule-refresh rs e update
    
    -- Render, imperatively updating the browser's DOM.
    vd ← get rvd
    vd' ← Renderer.darn vd doc  =<< Cursor.init parent
    Renderer.re-style vd'       =<< Cursor.init parent
    -- Store the vDOM tracking the new browser DOM
    set rvd vd'
  -- Add this view's rendering callback to the store
  -- so that updates due to other views will also refresh this view
  modify (rs .on-animation-tick-hooks) (renderThisView ∷_)
  -- Render the initial view (replaces el & sets up callbacks)
  renderThisView

  forM_ (e0 .actions) λ action →
    action λ e → apply-update-and-schedule-refresh rs e update
  where open Effect
