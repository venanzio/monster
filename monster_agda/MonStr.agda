{-# OPTIONS --without-K --guardedness --termination-depth=3 #-}

open import Categories.Category -- using (Category; _[_≈_])
open import Categories.Category.Instance.Sets
open import Categories.Category.Monoidal.Instance.Sets
open import Categories.Functor.Monoidal using (IsMonoidalFunctor; MonoidalFunctor)
open import Categories.Functor hiding (id)
open import Categories.Functor.Bifunctor
open import Categories.Functor.Coalgebra
open import Data.Product hiding (_<*>_)
open import Function using (_∘′_)
open import Categories.Category.Product
open import Relation.Binary using (Rel)

open import Relation.Binary.PropositionalEquality as ≡ using (_≡_; trans; cong; cong-app; refl)
open ≡.≡-Reasoning
open import Level renaming (suc to lsuc; zero to lzero)
open Functor
open Category

-- Category.Instance.Sets is useful to look at to get bearings on how to approach reasoning

-- Function extensionality axiom
postulate
  extensionality : ∀ {o}{A B : Set o}{f g : A → B} → Sets o [ f ≈ g ] → f ≡.≡ g

-- Containers
-- ======================================

-- Singleton set
record ⊤ {o : Level} : Set o where
  instance constructor tt

-- Could try to define this in full generality for locally CCCs
record Container (o : Level) : Set (lsuc o) where
  field
    Shapes : Set o
    Positions : Shapes → Set o

open Container


variable
  o : Level

-- Type mapping of the functor based on a container
M : ∀ {C : Container o}(A : Set o) → Set o
M {C = C} A = Σ (Shapes C) (λ s → Positions C s → A)


-- The full data of a functor based on a container
C→F : Container o → Endofunctor (Sets o)
C→F {o} C = record
  { F₀           = M {C = C}
  ; F₁           = ContainerFmap
  ; identity     = ≡.refl
  ; homomorphism = ≡.refl
  ; F-resp-≈     = ContainerFmapResp≈
  }
  where
    ContainerFmap : ∀ {A B : Set o} → Sets o [ A , B ] → Sets o [ M A ,  M B ]
    ContainerFmap f (s , p) = s , f ∘′ p

    ContainerFmapResp≈ : ∀ {A B : Set o} {f g : Sets o [ A , B ]} → Sets o [ f ≈ g ] → Sets o [ ContainerFmap f ≈ ContainerFmap g ]
    ContainerFmapResp≈ {A} {B} {f} {g} f≈g {s , p} = ≡.cong (λ h → s , h ∘′ p) (extensionality {f = f} {g = g} f≈g)


-- Lift binary relation to one on containers
data CRel {C : Container o}{A : Set o}(R : Rel A o) : Rel (M A) o where
  crel : ∀ {s : Shapes C}{h1 h2 : Positions C s → A} → (∀ {p : Positions C s} → R (h1 p) (h2 p)) → CRel R (s , h1) (s , h2)
  

-- Lift binary relation to one on products
prlift : ∀ {A B : Set o}(RA : Rel A o)(RB : Rel B o) → Rel (A × B) o
prlift ra rb = λ (a₁ , b₁) (a₂ , b₂) → (ra a₁ a₂) × (rb b₁ b₂)


-- Definition of monadic stream data type
-- ======================================
record MonStr (C : Container o) (A : Set o) : Set o where
  coinductive
  constructor mcons 
  field
    unmcons : M {C = C} (A × MonStr C A)

open MonStr


-- Bisimulation definition
record _∼∼_ {C : Container o}{A : Set o}(m₁ m₂ : MonStr C A) : Set o where
  coinductive
  constructor mbisim
  field
    unmbisim : CRel (prlift (_≡_) (_∼∼_)) (unmcons m₁) (unmcons m₂)

open _∼∼_


-- Coinduction axiom
postulate
  coinduction : ∀ {C : Container o}{A : Set o}{m₁ m₂ : MonStr C A} → m₁ ∼∼ m₂ → m₁ ≡ m₂


-- Pair functoriality proofs
-- ==========================

-- A helper function to prove equality of pairs when they are equal in each projection
pair≡ : ∀ {o}{A B : Set o}{a₁ a₂ : A}{b₁ b₂ : B} →  -- If ...
          (∀ (b : B) → ( a₁ , b ) ≡ ( a₂ , b )) →   -- π₁ is equal
          (∀ (a : A) → ( a , b₁ ) ≡ ( a , b₂ )) →   -- π₂ is equal
          ( a₁ , b₁ ) ≡ ( a₂ , b₂ )                 -- ... then the pair is equivalent
pair≡ {a₁ = a₁} {b₂ = b₂} π₁≡ π₂≡ = trans (π₂≡ a₁) (π₁≡ b₂)

    
-- Monster proofs
-- ====================

-- Proof that Monsters are Functors
MonStrF : Container o → Endofunctor (Sets o)
MonStrF {o} C with C→F C
... | F = record
  { F₀           = MonStr C         -- action on 0-cells
  ; F₁           = MonStrFMap       -- action on 1-cells
  ; identity     = coinduction lem1 -- proof that id morphisms are conserved
  ; homomorphism = coinduction lem2 -- proof that a→b maps to Fa→Fb
  ; F-resp-≈     = lem3             -- proof that F respects isomorphism
  }
  where

    MonStrFMap : ∀ {A B : Set o} → Sets o [ A , B ] → Sets o [ MonStr C A , MonStr C B ]
    unmcons (MonStrFMap {A = A} {B = B} f ma) with unmcons ma
    ... | (s , p) = ( s , (λ x → (λ (a , b) → (f a , MonStrFMap f b)) (p x)))

    lem1 : {A : Set o}{ma : MonStr C A} → MonStrFMap {A} {A} (id (Sets o)) ma ∼∼ id (Sets o) ma
    unmbisim (lem1 {A} {ma}) with unmcons ma
    ... | s , p = crel (λ {x} → ≡.refl , lem1 {A} {proj₂ (p x)})

    lem2 : {X Y Z : Set o}{f : Sets o [ X , Y ]}{g : Sets o [ Y , Z ]} →
         {ma : MonStr C X} → MonStrFMap (Sets o [ g ∘ f ]) ma ∼∼ (Sets o [ MonStrFMap g ∘ MonStrFMap f ]) ma
    unmbisim (lem2 {X} {Y} {Z} {f} {g} {ma}) with unmcons ma
    ... | s , p = crel (λ {x} → ≡.refl , lem2)

    lem3 : {A B : Set o}{f g : Sets o [ A , B ]} → Sets o [ f ≈ g ] → Sets o [ MonStrFMap f ≈ MonStrFMap g ]
    lem3 {A} {B} {f} {g} f≈g {ma} = coinduction lem3a
      where
        lem3a : {ma : MonStr C A} → MonStrFMap f ma ∼∼ MonStrFMap g ma
        unmbisim (lem3a {ma}) with unmcons ma
        ... | s , p = crel (λ {x} → f≈g , lem3a)


record Applicative {o : Level}(F : Endofunctor (Sets o)) : Set (lsuc o) where
  field
    pure : {A : Set o}(a : A) → F₀ F A
    _<*>_ : {A B : Set o} → F₀ F (A → B) → F₀ F A → F₀ F B

    identity : {A B : Set o}{a : F₀ F A} → (pure (id (Sets o)) <*> a) ≡ a
    homomorphism : {A B : Set o}{f : A → B}{a : A} → (pure f <*> pure a) ≡ pure (f a)
    interchange : {A B : Set o}{mf : F₀ F (A → B)}{a : A} → (mf <*> pure a) ≡ pure (λ f → f a) <*> mf
    composition : {X Y Z : Set o}{mg : F₀ F (Y → Z)}{mf : F₀ F (X → Y)}{mx : F₀ F X} → mg <*> (mf <*> mx) ≡ ((pure (_∘_ (Sets o)) <*> mg) <*> mf) <*> mx

    --fmap≡ : {A B : Set o}{f : A → B} → (pure f) <*>_ ≡ F₁ F f


record Monoid {o : Level}(S : Set o) : Set o where

  eta-equality

  field
    unit : S
    _*_ : S → S → S

    unitl : {a : S} → unit * a ≡ a
    unitr : {a : S} → a * unit ≡ a
    assoc : {a b c : S} → a * (b * c) ≡ (a * b) * c


-- The full data of a functor based on a container
{-
C→F : {C : Container} → (Monoid (Shapes C)) → Applicative (C→F C)
C→F {o} C = record
  { F₀           = M {C = C}
  ; F₁           = ContainerFmap
  ; identity     = ≡.refl
  ; homomorphism = ≡.refl
  ; F-resp-≈     = ContainerFmapResp≈
  }
  where
    ContainerFmap : ∀ {A B : Set o} → Sets o [ A , B ] → Sets o [ M A ,  M B ]
    ContainerFmap f (s , p) = s , f ∘′ p

    ContainerFmapResp≈ : ∀ {A B : Set o} {f g : Sets o [ A , B ]} → Sets o [ f ≈ g ] → Sets o [ ContainerFmap f ≈ ContainerFmap g ]
    ContainerFmapResp≈ {A} {B} {f} {g} f≈g {s , p} = ≡.cong (λ h → s , h ∘′ p) (extensionality {f = f} {g = g} f≈g)

-}

-- Positions C (s * s') → Positions C s × Position C s'
-- this could be a comonoid generated by a monoid and a dependant type?
record DepComonoid {o : Level}{S : Set o}(m : Monoid S)(P : (s : S) → Set o) : Set (lsuc o) where

  open Monoid m

  field
    --counit : {s : S} → P s → P (unit) -- is this even needed?
    split : {s s' : S} → P (s * s') → P s × P s'

    -- The DepComonoid type mapping should preserve the monoid structure
    unitl : {s : S} → P (unit * s) ≡ P s -- might need to be only up to isomorphism to be proveable?
    unitr : {s : S} → P (s * unit) ≡ P s -- same here
    assoc : {a b c : S} → P (a * (b * c)) ≡ P ((a * b) * c)
{--
    unitl : {s s' : S}{p : P (_*_ m s s')} → 

    unitl : (a : S) → proj₁ (split a) ≡ a
    unitr : (a : S) → proj₂ (split a) ≡ a
    assoc : (a : S) → split (split a) ≡ (a * b) * c
 --}


module _ {o : Level}{C : Container o} where

  open Container C

  containerAppMon : Applicative (C→F C) → Σ (Monoid (Shapes C)) (λ m → DepComonoid m (Positions C))
  proj₁ (containerAppMon appC) = record
    { unit  = proj₁ (pure tt)
    ; _*_   = λ s s' → proj₁ ((s , λ _ x → x) <*> (s' , λ _ → tt))
    ; unitl = {!!}
    ; unitr = {!!}
    ; assoc = {!!}
    }
    where
      open Applicative appC
  proj₂ (containerAppMon appC) = record
    { split = {!!}
    ; unitl = {!!}
    ; unitr = {!!}
    ; assoc = {!!}
    }
    where
      open Monoid (proj₁ (containerAppMon appC))
      open Applicative appC
    
      

  containerApp' : Σ (Monoid (Shapes C)) (λ m → DepComonoid m (Positions C)) → Applicative (C→F C)
  containerApp' (m , dc) = record
    { pure         = λ a → (unit , λ _ → a)
    ; _<*>_        = λ (sf , pf) (sa , pa) → (sf * sa , λ pfa → bimapapp (pf , pa) (split pfa))
    ; identity     = {!!}
    ; homomorphism = {!!}
    ; interchange  = {!!}
    ; composition  = {!!}
    }
    where
      open Monoid m
      open DepComonoid dc
    
      bimapapp : {A B C D : Set o} → (A → (C → D)) × (B → C) → A × B → D
      bimapapp (f , g) (a , c) = (f a) (g c)
  
      lem1 : {A B : Set o} {a : F₀ (C→F C) A} → (unit * (proj₁ a) , (λ pfa → bimapapp ((λ _ → id (Sets o)) , proj₂ a) (split pfa))) ≡ a
      lem1 {A} {B} {(s , p)} = {!!}
  
      {-
      containerPure : (appC : Applicative (C→F C)) → Σ (Shapes C) (λ s → {A : Set o}{a : A} → pure a ≡ (s , λ _ → a))
      proj₁ (containerPure appC) = proj₁ (pure tt)
      proj₂ (containerPure appC) {A} {a} with pure a
      ... | (s , p) = {!!}
      -}


    
