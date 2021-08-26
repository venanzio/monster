{-# OPTIONS --without-K --safe --guardedness --termination-depth=2 #-}

open import Categories.Category -- using (Category; _[_≈_])
open import Categories.Category.Instance.Sets
open import Categories.Functor hiding (id)
open import Categories.Functor.Bifunctor
open import Categories.Functor.Coalgebra
open import Data.Product
open import Function using (_∘′_) renaming (id to idf)
open import Categories.Category.Product

import Relation.Binary.PropositionalEquality as ≡ using (_≡_; trans; cong; cong-app; refl)
open import Level renaming (suc to lsuc; zero to lzero)
open Functor
open Category


-- Category.Instance.Sets is useful to look at to get bearings on how to approach reasoning

-- A helper function to prove equality of pairs when they are equal in each projection
pair≡ : ∀ {o}{A B : Set o}{a₁ a₂ : A}{b₁ b₂ : B} →  -- If ...
          (∀ (b : B) → ( a₁ , b ) ≡.≡ ( a₂ , b )) → -- π₁ is equal
          (∀ (a : A) → ( a , b₁ ) ≡.≡ ( a , b₂ )) → -- π₂ is equal
          ( a₁ , b₁ ) ≡.≡ ( a₂ , b₂ )               -- ... then the pair is equivalent
pair≡ {a₁ = a₁} {b₂ = b₂} π₁≡ π₂≡ = ≡.trans (π₂≡ a₁) (π₁≡ b₂)


-- Proof that cartesian product is functorial in both arguments
-- ============================================================

-×- : ∀ {o} → Bifunctor (Sets o) (Sets o) (Sets o)
-×- = record
  { F₀           = λ A⊗B → proj₁ A⊗B × proj₂ A⊗B
  ; F₁           = ProductBiMap
  ; identity     = ≡.refl
  ; homomorphism = ≡.refl
  ; F-resp-≈     = ProductBiMapResp≈
  }
  where
    ProductBiMap : ∀ {o} {A×B C×D : Set o × Set o} →
                     (Product (Sets o) (Sets o) ⇒ A×B) C×D →
                     (Sets o ⇒ (proj₁ A×B × proj₂ A×B)) (proj₁ C×D × proj₂ C×D)
    ProductBiMap (f , g) (a , b) = f a , g b

    ProductBiMapResp≈ : ∀ {o} → {A×B C×D : Set o × Set o}
                          {f g : Product (Sets o) (Sets o) [ A×B , C×D ]} →
                          Product (Sets o) (Sets o) [ f ≈ g ] →
                          Sets o [ ProductBiMap f ≈ ProductBiMap g ]
    ProductBiMapResp≈ {o = o} (f₁≈g₁ , f₂≈g₂) {a , b} =
                          pair≡ (λ y → ≡.cong (λ ha → ha , y) f₁≈g₁)
                                (λ x → ≡.cong (λ hb → x , hb) f₂≈g₂)
                                

-×_ : ∀ {o} → Set o → Endofunctor (Sets o)
-×_ {o} X = record
  { F₀           = λ A → A × X
  ; F₁           = λ f → (λ (a , x) → (f a , x))
  ; identity     = ≡.refl
  ; homomorphism = ≡.refl
  ; F-resp-≈     = ProductResp≈
  }
  where
    ProductResp≈ : ∀ {A B : Set o} {f g : Sets o [ A , B ]} →
                     Sets o [ f ≈ g ] →
                     Sets o [(λ x → f (proj₁ x) , proj₂ x) ≈ (λ x → g (proj₁ x) , proj₂ x)]
    ProductResp≈ f≈g {a , x} = ≡.cong (λ ha → ha , x) f≈g


_×- : ∀ {o} → Set o → Endofunctor (Sets o)
_×- {o} X = record
  { F₀           = λ A → X × A
  ; F₁           = λ f → (λ (x , a) → (x , f a))
  ; identity     = ≡.refl
  ; homomorphism = ≡.refl
  ; F-resp-≈     = ProductResp≈
  }
  where
    ProductResp≈ : ∀ {A B : Set o} {f g : Sets o [ A , B ]} →
                     Sets o [ f ≈ g ] →
                     Sets o [(λ x → proj₁ x , f (proj₂ x)) ≈ (λ x → proj₁ x , g (proj₂ x))]
    ProductResp≈ f≈g {x , a} = ≡.cong (λ ha → x , ha) f≈g


-- Container functors
-- ======================================

-- Could try to define this in full generality for any locally CCC
record Container (o : Level) : Set (lsuc o) where
  field
    Shapes : Set o
    Positions : Shapes → Set o

open Container


lem1 : ∀ {o}{A B C : Set o}{f g : A → B}{p : C → A} → (∀ {x} → f x ≡.≡ g x) → (∀ {x} → f (p x) ≡.≡ g (p x))
lem1 {o} {A} {B} {C} {f} {g} {p} f≈g {x} = f≈g

lem2 : ∀ {o}{A B C : Set o}{f g : A → B}{p : C → A} → (∀ {x} → f (p x) ≡.≡ g (p x)) → ((λ x → f (p x)) ≡.≡ (λ x → g (p x)))
lem2 {o} {A} {B} {C} {f} {g} {p} f≈g = {!!}


C→F : ∀ {o} → Container o → Endofunctor (Sets o)
C→F {o} C = record
  { F₀           = λ A → Σ (Shapes C) (λ s → Positions C s → A)
  ; F₁           = ContainerFmap
  ; identity     = ≡.refl
  ; homomorphism = ≡.refl
  ; F-resp-≈     = ContainerFmapResp≈
  }
  where
    ContainerFmap : ∀ {A B : Set o} → Sets o [ A , B ] → Sets o [  Σ (Shapes C) (λ s → Positions C s → A) ,  Σ (Shapes C) (λ s → Positions C s → B) ]
    ContainerFmap f (s , p) = s , (λ x → f (p x))

    ContainerFmapResp≈ : ∀ {A B : Set o} {f g : Sets o [ A , B ]} → Sets o [ f ≈ g ] → Sets o [ ContainerFmap f ≈ ContainerFmap g ]
    ContainerFmapResp≈ {A} {B} {f} {g} f≈g {s , p} =
      begin
        s , (λ x → f (p x))
      ≡⟨ {!!} ⟩
        s , (λ x → g (p x))
      ∎
      where
        open import Relation.Binary.PropositionalEquality
        open ≡-Reasoning

    

-- Definition of monadic stream data type
-- ======================================

record MonStr {o : Level} (M : Container o) (A : Set o) : Set o where
  coinductive
  constructor mcons 
  field
    unmcons : F₀ (C→F M) (A × MonStr M A)

open MonStr


-- Proof that Monsters are Coalgebras
MonStrCoAlg : ∀ {o} → (M : Container o) (A : Set o) → F-Coalgebra ((C→F M) ∘F (A ×-))
MonStrCoAlg M A = record
  { A = MonStr M A
  ; α = unmcons
  }


-- Proof that Monsters are Functors
MonStrF : ∀ {o} → Container o → Endofunctor (Sets o)
MonStrF {o} M = record
  { F₀           = λ A → MonStr M A -- action on 0-cells
  ; F₁           = MonStrFMap -- action on 1-cells
  ; identity     = {!!} -- proof that id morphisms are conserved
  ; homomorphism = {!!} -- proof that a->b maps to Fa->Fb
  ; F-resp-≈     = {!!} -- proof that F respects isomorphism
  }
  where
    MonStrFMap : ∀ {A B : Set o} → Sets o [ A , B ] → Sets o [ MonStr M A , MonStr M B ]
    unmcons (MonStrFMap {A = A} {B = B} f ma) with unmcons ma
    ... | mp = F₁ ((C→F M) ∘F -×-) (f , MonStrFMap f) mp

    -- Start with pure stream proof below to get used to coinductive proofs
    --MonStrFMapId : ∀ {A : Set o} → Sets o [ MonStrFMap (id (Sets o)) ≈ id (Sets o) ]
    --MonStrFMapId {A} {x} = {!!}





record Stream {o : Level} (A : Set o) : Set o where
  coinductive
  constructor cons
  field
    uncons : A × Stream A

open Stream

StreamF : ∀ {o} → Endofunctor (Sets o)
StreamF {o} = record
  { F₀           = λ A → Stream A -- action on 0-cells
  ; F₁           = StreamFmap -- action on 1-cells
  ; identity     = {!!} -- proof that id morphisms are conserved
  ; homomorphism = {!!} -- proof that a->b maps to Fa->Fb
  ; F-resp-≈     = {!!} -- proof that F respects isomorphism
  }
  where
    StreamFmap : ∀ {A B : Set o} → Sets o [ A , B ] → Sets o [ Stream A , Stream B ]
    uncons (StreamFmap {A = A} {B = B} f sa) with uncons sa
    ... | (a , sa') = f a , StreamFmap f sa'
