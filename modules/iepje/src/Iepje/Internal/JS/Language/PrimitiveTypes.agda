
-- Agda bindings to primitive JavaScript types,
-- https://developer.mozilla.org/en-US/docs/Glossary/Primitive

module Iepje.Internal.JS.Language.PrimitiveTypes where

import Agda.Builtin.Float
import Agda.Builtin.Int
import Agda.Builtin.String
import Agda.Builtin.Bool

-- > In JavaScript, a primitive (primitive value, primitive data type) is data
-- > that is not an object and has no methods or properties.
-- > There are 7 primitive data types:

string = Agda.Builtin.String.String -- known by Agda JS backend

number = Agda.Builtin.Float.Float -- known by Agda JS backend

bigint = Agda.Builtin.Int.Int -- known by Agda JS backend

boolean = Agda.Builtin.Bool.Bool -- known by Agda JS backend

postulate undefined : Set

postulate symbol : Set

postulate null : Set
