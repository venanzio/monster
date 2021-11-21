{-# OPTIONS --without-K --guardedness --termination-depth=3 #-}

open import Data.Product
open import Relation.Binary.PropositionalEquality using (_≡_; trans; cong; cong-app; refl)
open Relation.Binary.PropositionalEquality.≡-Reasoning
open import Level renaming (suc to lsuc; zero to lzero)
open import Relation.Binary using (Rel)

-- Function extensionality axiom
postulate
  extensionality : ∀ {o}{A B : Set o}{f g : A → B} → (∀ x → f x ≡ g x) → f ≡ g

id : ∀ {o : Level}(A : Set o) → A → A
id A x = x

_∘_ : ∀ {o : Level}{X Y Z : Set o} → (Y → Z) → (X → Y) → X → Z
(g ∘ f) x = g (f x)
{-# INLINE _∘_ #-}

record Container (o : Level) : Set (lsuc o) where
  field
    Shapes : Set o
    Positions : Shapes → Set o

open Container


-- Type mapping of the functor based on a container
M : ∀ {o : Level}{C : Container o}(A : Set o) → Set o
M {C = C} A = Σ (Shapes C) (λ s → Positions C s → A)


record CFunctor {o : Level}(C : Container o) : Set (lsuc o) where
  field
    F₀ : (A : Set o) → M {o} {C} A
    F₁ : {A B : Set o} → (A → B) → (M {o} {C} A → M {o} {C} B)
    identity : {A : Set o} → F₁ (id A) ≡ id (M {o} {C} A)
    homomorphism : {X Y Z : Set o} {f : X → Y} {g : Y → Z} → F₁ (g ∘ f) ≡ F₁ g ∘ F₁ f
    F-resp-≡ : {A B : Set o}{f g : A → B} → f ≡ g → F₁ f ≡ F₁ g

open CFunctor


record CApplicative {o : Level}(C : Container o) : Set (lsuc o) where
  field
    F : CFunctor C


record Functor {o : Level} : Set (lsuc o) where
  field
    F₀ : Set o → Set o
    F₁ : {A B : Set o} → (A → B) → (F₀ A → F₀ B)
    identity : {A : Set o} → F₁ (id A) ≡ id (F₀ A)
    homomorphism : {X Y Z : Set o} {f : X → Y} {g : Y → Z} → F₁ (g ∘ f) ≡ F₁ g ∘ F₁ f
    F-resp-≡ : {A B : Set o}{f g : A → B} → f ≡ g → F₁ f ≡ F₁ g


record MonStr {o : Level}{C : Container o}(F : CFunctor C)(A : Set o) : Set o where
  coinductive
  constructor mcons 
  field
    unmcons : M {o} {C} (A × MonStr F A)

open MonStr


-- Lift binary relation to one on containers
--data CRel {C : Container o}{A : Set o}(R : Rel A o) : Rel (M A) o where
--  crel : ∀ {s : Shapes C}{h1 h2 : Positions C s → A} → (∀ {p : Positions C s} → R (h1 p) (h2 p)) → CRel R (s , h1) (s , h2)

--open CRel

-- Lift binary relation to one on products
--prlift : ∀ {A B : Set o}(RA : Rel A o)(RB : Rel B o) → Rel (A × B) o
--prlift ra rb = λ (a₁ , b₁) (a₂ , b₂) → (ra a₁ a₂) × (rb b₁ b₂)

-- Bisimulation definition
--record _∼∼_ {o : Level}{C : Container o}{F : CFunctor o}{A : Set o}(m₁ m₂ : MonStr F A) : Set-- o where
--  coinductive
--  constructor mbisim
--  field
--    unmbisim : CRel (prlift (≡._≡_) (_∼∼_ {o}{C}{A})) (unmcons m₁) (unmcons m₂)


-- Coinduction axiom
--postulate
--  coinduction : ∀ {C : Container o}{A : Set o}(m₁ m₂ : MonStr C A) → m₁ ∼∼ m₂ → m₁ ≡.≡ m₂

MonStrF : {o : Level}{C : Container o} → CFunctor C → Functor
MonStrF {o} F = record
  { F₀           = MonStr F
  ; F₁           = {!!}
  ; identity     = {!!} -- proof that id morphisms are conserved
  ; homomorphism = {!!} -- proof that a->b maps to Fa->Fb
  ; F-resp-≡     = {!!} -- proof that F respects isomorphism
  }
  where
    MonStrFMap : {A B : Set o} → (A → B) → MonStr F A → MonStr F B
    MonStrFMap f ma = mcons (F₁ F (λ (a , s) → (f a , MonStrFMap f s)) (unmcons ma))

    --MonStrFMapId : {A : Set o} → ∀ {ma : MonStr F A} → MonStrFMap (id A) ma ≡ id (MonStr F A) ma
    --MonStrFMapId {A} {ma} =
    --  begin
    --    mcons (F₁ F (λ (a , s) → ((id A) a , MonStrFMap (id A) s)) (unmcons ma))
    --  ≡⟨⟩
    --    ?
