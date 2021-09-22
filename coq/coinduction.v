(* Abstract formalization of coinduction *)

(* Instead of using functors that are syntactically strictly positive (containers)
   we characterize abstractly the existence of a final coalgebra.
*)

(* Identity and composition operators (are they not in the library?) *)

Definition id (A:Set): A -> A := fun x => x.

Definition comp {A B C:Set}(g:B->C)(f:A -> B): A -> C :=
fun x => g (f x).

(* Definition of functor on Set *)

Record Functor: Type:=
  { fun_obj: Set -> Set;
    fun_morph: forall {A B:Set}, (A->B) -> fun_obj A -> fun_obj B;
    fun_id_law: forall (A:Set), fun_morph (id A) = id (fun_obj A);
    fun_comp_law: forall (A B C:Set)(f:A->B)(g:B->C),
                    fun_morph (comp g f) = comp (fun_morph g) (fun_morph f)
  }.

Parameter F: Functor.

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
  { bisim_rel: Coalgebra;
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

(* We call a coalgebra coinductive if
   bisimulation implies equality.
*)

Definition Coinductive (coalg:Coalgebra): Type :=
  forall (R:Bisimulation coalg coalg),
    forall r: bisim_rel R, bisim_p1 R r = bisim_p2 R r.

(* A final coalgebra is weakly final and coinductive *)

Record Final (fcoalg: Coalgebra): Type :=
  { ana_morphism: WeaklyFinal fcoalg;
    coinductive: Coinductive fcoalg
  }.

