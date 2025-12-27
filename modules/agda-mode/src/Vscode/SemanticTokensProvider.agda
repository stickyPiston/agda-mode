module Vscode.SemanticTokensProvider where

open import Prelude.List
open import Prelude.JSON
open import Prelude.Sigma
open import Prelude.Maybe
open import Agda.Builtin.String
open import TEA.System
open System
open import Iepje.Internal.JS.Language.IO
open import Agda.Builtin.Unit
open import TEA.Capability
open import TEA.Cmd

postulate EventEmitter : Set

postulate new-event-emitter : IO EventEmitter

postulate fire : EventEmitter → IO ⊤

data LanguageFilter : Set where
    language scheme path-pattern : String → LanguageFilter
    _∩_ : LanguageFilter → LanguageFilter → LanguageFilter

encode-language-filter : LanguageFilter → JSON
encode-language-filter filter = j-object (kvs filter)
    where
        kvs : LanguageFilter → List (String × JSON)
        kvs (language x) = [ "language" , j-string x ]
        kvs (scheme x) = [ "scheme" , j-string x ]
        kvs (path-pattern x) = [ "pattern" , j-string x ]
        kvs (l ∩ r) = kvs l ++ kvs r

postulate Legend SemanticToken : Set

postulate mk-Legend : List String → List String → vscode-api → Legend
{-# COMPILE JS mk-Legend = types => mods => vscode => new vscode.SemanticTokensLegend(types, mods) #-}

postulate Document CancellationToken : Set

-- TODO: Handle cancellations
-- TODO: resolve takes the output of SemanticTokensBuilder.build()
postulate register-semantic-tokens-provider : vscode-api → JSON → EventEmitter → (Document → CancellationToken → (return : List SemanticToken → IO ⊤) → IO ⊤) → Legend → IO Disposable
{-# COMPILE JS register-semantic-tokens-provider = vscode => selector => onChangeEmitter => provider => legend => vscode.languages.registerDocumentSemanticTokensProvider(
    selector,
    {
        onDidChangeSemanticTokens: onChangeEmitter.event,
        provideDocumentSemanticTokens: (document, token) => new Promise((resolve, reject) => { provider(document)(token)(resolve)(() => {}) }),
    },
    legend
) #-}

semantic-tokens-provider :
    ∀ {msg}
    → (vscode-api → Legend)
    → ((List SemanticToken → Cmd msg) → msg)
    → LanguageFilter
    → Capability msg
semantic-tokens-provider {msg} legend on-request-msg selector = record
    { requirement-type = EventEmitter
    ; new-requirement = new-event-emitter
    ; provided-type = just (Cmd msg , λ on-change-emitter → mk-Cmd λ  _ → fire on-change-emitter)
    ; register = λ system requirement update →
        let vscode = system .vscode
            provider = λ doc token return → update (on-request-msg λ tokens → mk-Cmd λ _ → return tokens)
         in register-semantic-tokens-provider vscode (encode-language-filter selector) requirement provider (legend vscode)
    }
