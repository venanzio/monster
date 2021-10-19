(* Abstract formalization of coinduction *)

(* Instead of using functors that are syntactically strictly positive (containers)
   we characterize abstractly the existence of a final coalgebra.
*)

Section Coinduction.

(* Identity and composition operators (are they not in the library?) *)

Definition id (A:Set): A -> A := fun x => x.

Definition comp {A B C:Set}(g:B->C)(f:A -> B): A -> C :=
fun x => g (f x).

(* Definition of functor on Set *)

Definition FunctorIdLaw (F:Set->Set)(fmorph:forall {A B:Set}, (A->B) -> F A -> F B): Prop :=
  forall (A:Set), fmorph (id A) = id (F A).

Definition FunctorCompLaw (F:Set->Set)(fmorph:forall {A B:Set}, (A->B) -> F A -> F B): Prop :=
  forall (A B C:Set)(f:A->B)(g:B->C),
    fmorph (comp g f) = comp (fmorph g) (fmorph f).

Record Functor: Type:= functor
  { fun_obj:> Set -> Set;
    fun_morph: forall {A B:Set}, (A->B) -> fun_obj A -> fun_obj B;
    fun_id_law: FunctorIdLaw fun_obj (@fun_morph);
    fun_comp_law: FunctorCompLaw fun_obj (@fun_morph)
  }.

Variable F: Functor.

(* F-coalgebras and coalgebra morphisms *)

Record Coalgebra: Type :=
  { elements:> Set;
    coalgebra: elements -> fun_obj F elements
  }.

Record CoalgMorphism (coalg1 coalg2: Coalgebra): Type :=
  { morphism:> elements coalg1 -> elements coalg2;
    coalg_morph_law: 
      comp (coalgebra coalg2) (morphism) = 
      comp (fun_morph F morphism) (coalgebra coalg1) 
  }.

Arguments morphism {coalg1 coalg2}.

(* A relation between sets is modeled by a span
   Idea: span_rel is a set of "certificates" that elements of A and B are related
   p:span_rel is evidence that (span_p1 p) and (span_p2 p) are related
*)

Record Span (A B:Set): Type :=
  { span_rel:> Set;
    span_p1: span_rel -> A;
    span_p2: span_rel -> B
  }.

(* Bisimulation:
   A bisimulation is a span in the category of coalgebras
*)

Record Bisimulation (coalg1 coalg2: Coalgebra): Type :=
  { bisim_rel:> Coalgebra;
    bisim_p1: CoalgMorphism bisim_rel coalg1;
    bisim_p2: CoalgMorphism bisim_rel coalg2
  }.

Arguments bisim_rel {coalg1 coalg2}.
Arguments bisim_p1 {coalg1 coalg2}.
Arguments bisim_p2 {coalg1 coalg2}.

(* A weakly final coalgeba is one for which
   there is a morphism from every coalgebra to it.
*)

Definition WeaklyFinal (fcoalg:Coalgebra): Type :=
  forall (coalg:Coalgebra), CoalgMorphism coalg fcoalg.

(* Uniqueness of morphisms to a given coalgebra from any coalgebra *)

Definition UniqueMorph (coalg:Coalgebra): Type :=
  forall (coalg0:Coalgebra)(f1 f2:CoalgMorphism coalg0 coalg),
    morphism f1 = morphism f2.

(* A final coalgebra is weakly final and satisfies uniqueness *)

Record Final (fcoalg: Coalgebra): Type :=
  { final_morphism: WeaklyFinal fcoalg;
    final_unique: UniqueMorph fcoalg
  }.

(* Unicity of morphisms implies the Coinduction Principle:
     bisimulation implies equality
*)

Lemma unique_bisim:
  forall (coalg:Coalgebra), UniqueMorph coalg ->
  forall (R:Bisimulation coalg coalg),
    forall r: bisim_rel R, bisim_p1 R r = bisim_p2 R r.
Proof.
intros coalg unique R r.
rewrite (unique (bisim_rel R) (bisim_p1 R) (bisim_p2 R)).
trivial.
Qed.


(* Elements of a coalgebra are bisimilar if there exists a bisimulation *)

Definition Bisimilar (coalg:Coalgebra)(x1 x2:coalg): Prop :=
  exists (R: Bisimulation coalg coalg)(r:R),
    x1 = bisim_p1 R r /\ x2 = bisim_p2 R r.

(* Coinduction Principle: in a final coalgebra
     bisimulation implies equality
*)

Theorem coinduction:
  forall (fcoalg: Coalgebra), Final fcoalg ->
    forall x1 x2: fcoalg, Bisimilar fcoalg x1 x2 -> x1 = x2.
Proof.
intros fcoalg final x1 x2 [R [r [h1 h2]]].
rewrite h1; rewrite h2.
apply (unique_bisim fcoalg).
apply (final_unique fcoalg final).
Qed.

End Coinduction.
