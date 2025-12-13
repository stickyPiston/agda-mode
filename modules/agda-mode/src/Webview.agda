module Webview where

open import Communication

open import Iepje.Prelude
open import Iepje.Internal.JS.Language.IO
open import Iepje.Internal.JS.Language.PrimitiveTypes
open import Iepje.Internal.JS.WebAPIs.DOM
open import Iepje.Internal.Effect

postulate onMessage : Window → (null → IO ⊤) → IO null
{-# COMPILE JS onMessage = win => dispatch => cont => { win.addEventListener("message", () => { console.log(dispatch); dispatch(null)(() => {}) }); cont(null) } #-}

data Cmd : Set where
    message-received button-pressed : Cmd

message-effect : Effect Cmd
message-effect = from λ dispatch → do
  w ← document >>= get-defaultView
  _ ← onMessage w λ _ → dispatch message-received
  pure tt

postulate internal-post-message : JSON → IO null
{-# COMPILE JS internal-post-message = msg => cont => { acquireVsCodeApi().postMessage(msg); cont(null) } #-}

postulate log : ∀ {A : Set} → A → IO null
{-# COMPILE JS log = _ => thing => cont => { console.log(thing); cont(null) } #-}

post-message : ∀ {A} → Msg → Effect A
post-message msg = from λ _ → do
    _ ← internal-post-message (encode msg)
    pure tt

main : IO ⊤
main = interactIO "main"
  (0 , message-effect)
  (λ n → pure do
    text $ primShowNat n ++ " messages received"
    button button-pressed $ text "send message")
  (λ cmd model → case cmd of λ where
    message-received → pure (1 + model , none)
    button-pressed → pure (model , post-message a))