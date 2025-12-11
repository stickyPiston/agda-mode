
-- Agda bindings to JavaScript's built-in "global objects"
-- (i.e. bindings available in global scope),
-- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects

module Iepje.Internal.JS.Language.GlobalObjects where

open import Iepje.Internal.JS.Language.IO
open import Iepje.Internal.JS.Language.PrimitiveTypes as Types hiding (null)

-- Some JS primitive types have similarly-named object types.
-- However, these are not interchangeable. See
-- https://javascriptweblog.wordpress.com/2010/09/27/the-secret-life-of-javascript-primitives/
postulate String Number BigInt Boolean Symbol : Set

-- Other global objects from the list at
-- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/
postulate Object : Set

-- Date

postulate now : IO number
{-# COMPILE JS now = kn => kn(Date.now()) #-}

-- null

postulate null : Types.null
