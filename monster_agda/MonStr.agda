{-# OPTIONS --without-K --safe #-}

open import Categories.Category -- using (Category; _[_≈_])
open import Categories.Category.Instance.Sets
open import Categories.Functor hiding (id)
open import Categories.Functor.Bifunctor
open import Categories.Functor.Coalgebra
open import Data.Product
open import Function using (_∘′_) renaming (id to idf)
open import Categories.Category.Product

import Relation.Binary.PropositionalEquality as ≡ using (_≡_; trans; cong; cong-app; refl)
open import Level
open Functor
open Category


-- Stuff related to Hom sets is (surprisingly) in Functor.Hom
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


-- Definition of monadic stream data type
-- ======================================

-- I think we need to specify that the endofunctor M is strictly positive in its argument for this
-- to pass positivity checking - this is almost definitely the case
{-# NO_POSITIVITY_CHECK #-} 
record MonStr {o : Level} (M : Endofunctor (Sets o)) (A : Set o) : Set o where
  constructor mcons 
  coinductive
  field
    uncons : F₀ M (A × MonStr M A)

open MonStr


-- Proof that Monsters are Coalgebras
MonStrCoAlg : ∀ {o} → (M : Endofunctor (Sets o)) (A : Set o) → F-Coalgebra (M ∘F (A ×-))
MonStrCoAlg M A = record
  { A = MonStr M A
  ; α = uncons
  }


-- Proof that Monsters are Functors
MonStrF : ∀ {o} → Endofunctor (Sets o) → Endofunctor (Sets o)
MonStrF {o} M = record
  { F₀           = λ A → MonStr M A -- action on 0-cells
  ; F₁           = MonStrFMap -- action on 1-cells
  ; identity     = {!!} -- proof that id morphisms are conserved
  ; homomorphism = {!!} -- proof that a->b maps to Fa->Fb
  ; F-resp-≈     = {!!} -- proof that F respects isomorphism
  }
  where
    MonStrFMap : ∀ {A B : Set o} → Sets o [ A , B ] → Sets o [ MonStr M A , MonStr M B ]
    MonStrFMap {A = A} {B = B} f =
      (mcons {o} {M} {B}) ∘′
        (F₁ M ((F₁ -×-) (f , MonStrFMap f))) ∘′
          (uncons {o} {M} {A}) -- This is a hylomorphism?

    MonStrFMapId : ∀ {A : Set o} → Sets o [ MonStrFMap (id (Sets o)) ≈ id (Sets o) ]
    MonStrFMapId = {!!}
