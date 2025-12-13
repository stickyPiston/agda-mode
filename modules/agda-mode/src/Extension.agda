module Extension where

open import Iepje.Internal.JS.Language.IO
open import Iepje.Internal.JS.Language.PrimitiveTypes
open import Iepje.Internal.JS.Language.MutableReferences
open import Iepje.Prelude hiding (Maybe; just; nothing)
open import Agda.Builtin.List
open import Agda.Builtin.Maybe
open import Communication

postulate
    Command : Set
    ExtensionContext : Set
    vsc : Set
    Panel : Set

    registerCommand : vsc → string → IO null → IO Command
    createWebviewPanel : vsc → IO Panel
    setHtml : string → Panel → IO null
    pushSubscription : Command → ExtensionContext → IO null
    extensionUri : ExtensionContext → string
    joinPath : vsc → List string → string
    toWebviewUri : Panel → string → string
    log : ∀{A : Set} → A → IO null
    postMessage : Panel → IO null
    onMessage : Panel → ExtensionContext → (JSON → IO null) → IO null

{-# COMPILE JS registerCommand = vscode => name => action => cont => cont(vscode.commands.registerCommand(name, () => { action(_ => {}) })) #-}
{-# COMPILE JS createWebviewPanel = vscode => cont => cont(vscode.window.createWebviewPanel("window", "window", vscode.ViewColumn.One, { enableScripts: true }))  #-}
{-# COMPILE JS pushSubscription = cmd => context => cont => { context.subscriptions.push(cmd); cont(null); } #-}
{-# COMPILE JS log = _ => thing => cont => { console.log(thing); cont(null) } #-}
{-# COMPILE JS extensionUri = context => context.extensionUri #-}
{-# COMPILE JS joinPath = vscode => parts => vscode.Uri.joinPath(...parts) #-}
{-# COMPILE JS toWebviewUri = panel => url => panel.webview.asWebviewUri(url) #-}
{-# COMPILE JS setHtml = html => panel => cont => { panel.webview.html = html; cont(null) } #-}
{-# COMPILE JS postMessage = panel => cont => { panel.webview.postMessage(null); cont(null) } #-}
{-# COMPILE JS onMessage = panel => ctx => action => cont => { panel.webview.onDidReceiveMessage(msg => action(msg)(() => {}), undefined, ctx.subscriptions); cont(null); } #-}

openWebviewPanel : vsc → ExtensionContext → Ref (Maybe Panel) → IO null
openWebviewPanel vscode context ref = do
    panel ← createWebviewPanel vscode
    _ ← set ref $ just panel
    _ ← setHtml ("<html><body><main></main><script type=\"module\" src="
        ++ toWebviewUri panel (joinPath vscode (extensionUri context ∷ "out" ∷ "jAgda.Webview.mjs" ∷ []))
        ++ "></script></body></html>") panel
    onMessage panel context λ json → case (decode json) of λ where
        (just a) → log "Received a!"
        (just b) → log "Received b!"
        nothing  → log "Could not parse message"

sendMessage : Ref (Maybe Panel) → IO null
sendMessage panel =
    get panel >>= λ where
        (just p) → postMessage p
        nothing → log "No panel set"

activate : vsc → ExtensionContext → IO null
activate vscode context = do
    panel ← new {Maybe Panel} nothing
    openPanelCmd ← registerCommand vscode "agda-mode.openPanel" (openWebviewPanel vscode context panel)
    _ ← pushSubscription openPanelCmd context
    sendMessageCmd ← registerCommand vscode "agda-mode.sendMessage" (sendMessage panel)
    pushSubscription sendMessageCmd context
