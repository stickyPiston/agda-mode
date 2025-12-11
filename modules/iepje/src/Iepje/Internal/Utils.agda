-- A module to dump misc. helper functions like `map`

module Iepje.Internal.Utils where

open import Iepje.Internal.JS.Language.IO

open import Agda.Builtin.Nat
open import Agda.Builtin.List
open import Agda.Builtin.Bool
open import Agda.Builtin.String
open import Agda.Builtin.Sigma
open import Agda.Builtin.Unit
open import Agda.Primitive

private variable
  ℓa ℓb ℓc : Agda.Primitive.Level
  A : Set ℓa
  B : Set ℓb
  C : Set ℓc
  B' : A → Set ℓb

-- Lists

map : (A → B) → List A → List B
map _ [] = []
map f (x ∷ xs) = f x ∷ map f xs

map' :  (Nat → A → B) → List A → List B
map' f = go 0 where
  go : Nat → List _ → List _
  go n [] = []
  go n (x ∷ l) = f n x ∷ go (suc n) l

length : List A → Nat
length [] = 0
length (_ ∷ xs) = 1 + length xs

for :  List A → (A → B) → List B
for l f = map f l

for' :  List A → (Nat → A → B) → List B
for' l f = map' f l

foldr : (A → B → B) → B → List A → B
foldr f z = go where
  go : _ → _
  go []         = z
  go (y ∷ ys) = f y (go ys)


-- Booleans

not : Bool → Bool
not false = true
not true  = false

_&&_ : Bool → Bool → Bool
true && true = true
_    && _    = false
infixl 3.5 _&&_

_||_ : Bool → Bool → Bool
false || false = false
_     || _      = true
infixl 3 _||_

-- Control flow

_$_ : ((a : A) → B' a) → (a : A) → B' a
f $ x = f x
infixr 1 _$_

_&_ : (a : A) → ((a : A) → B' a) → B' a
x & f = f x

_∘_ : (B → C) → (A → B) → (A → C)
(g ∘ f) x = g (f x)

case_of_ : A → (A → B) → B
case x of f = f x

const : A → B → A
const a _ = a

if_then_else_ : Bool → A → A → A
if true  then t else _ = t
if false then _ else e = e
infixr 20 if_then_else_

-- Nats

enumerate : Nat → List Nat
enumerate zero = []
enumerate (suc n) = n ∷ enumerate n

min : Nat → Nat → Nat
min m n = if m < n then m else n

max : Nat → Nat → Nat
max m n = if m < n then n else m

_/_ : Nat → Nat → Nat
n / m = div-helper 0 (m - 1) n (m - 1)
infixl 22 _/_

_%_ : Nat → Nat → Nat
n % m = mod-helper 0 (m - 1) n (m - 1)
infixl 22 _%_

-- Strings

_++_ = primStringAppend
infixl 20 _++_

-- Product

_×_ : Set → Set → Set
A × B = Σ A λ _ → B

-- IO

-- Various generalizations of _$_
-- (Haskell naming convention)

_<$>_ : (A → B) → IO A → IO B
f <$> ma = do a ← ma; pure (f a)
infixr 21 _<$>_

_<$_ : A → IO B → IO A
a <$ fb = const a <$> fb

_<&>_ : IO A → (A → B) → IO B
x <&> f = f <$> x

_>>_ : IO A → IO B → IO B
ma >> mb = do _ ← ma; mb

_<<_ : IO A → IO B → IO A
ma << mb = mb >> ma

_<*>_ : IO (A → B) → IO A → IO B
mf <*> mx = do f ← mf; x ← mx; pure (f x)
infixl 20 _<*>_

_<*_ : IO A → IO B → IO A
fa <* fb = const <$> fa <*> fb

_=<<_ : (A → IO B) → IO A → IO B
fmb =<< ma = ma >>= fmb

_<=<_ : (B -> IO C) -> (A -> IO B) -> A -> IO C
(fmc <=< fmb) a = do
  b ← fmb a
  c ← fmc b
  pure c

-- Helper for type of _$$_
IO? : Bool → Set → Set
IO? false A =    A
IO? true  A = IO A

-- Helper for body of _$$_
from-IO? : ∀ b → IO? b A → IO A
from-IO? false = pure
from-IO? true ma = ma

-- Operator generalizing _$_ _<$>_ _<*>_ _=<<_ flap etc.
_$$_ : ∀{x y z} → IO? x (A → IO? y B) → IO? z A → IO B
_$$_ {x = x}{y = y}{z = z} mfm ma = do
  fm ← from-IO? x mfm
  a ← from-IO? z ma
  from-IO? y (fm a)

infixl 20 _$$_

-- Other IO

void : IO A → IO ⊤
void m = m >> pure tt

sequenceA : List (IO A) → IO (List A)
sequenceA [] = pure []
sequenceA (x ∷ xs) = _∷_ <$> x <*> sequenceA xs

forM : List A → (A → IO B) → IO (List B)
forM as f = sequenceA $ map f as

forM_ : List A → (A → IO ⊤) → IO ⊤
forM_ = (void ∘_) ∘ forM