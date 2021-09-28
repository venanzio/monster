Require Export FunctionalExtensionality.

(* Replace this path with the location on your system *)
Add LoadPath "/home/venanzio/monster/coq".

Load coinduction.

(* Pure Streams *)

Definition StrFunObj (A:Set) : Set -> Set := fun X => prod A X.

Definition str_fun_morph (A:Set){X Y:Set}: (X->Y) -> StrFunObj A X -> StrFunObj A Y :=
  fun f x => pair (fst x) (f (snd x)).

Lemma str_fun_id_law: forall (A:Set), FunctorIdLaw (StrFunObj A) (@str_fun_morph A).
Proof.
intros A X.
apply functional_extensionality.
intros [a x].
auto.
Qed.
  
Lemma str_fun_comp_law: forall (A:Set), FunctorCompLaw (StrFunObj A) (@str_fun_morph A).
Proof.
intros A X Y Z f g.
apply functional_extensionality.
intros [a x].
auto.
Qed.

Definition StrFun (A:Set): Functor :=
  {| fun_obj := StrFunObj A;
     fun_morph := @str_fun_morph A;
     fun_id_law := str_fun_id_law A;
     fun_comp_law := str_fun_comp_law A
  |}.


