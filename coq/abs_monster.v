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

(* Monadic Streams *)


Variable M: Functor.

Definition MonStrFunObj (A:Set) : Set -> Set := fun X => M (StrFun A X).

Definition mon_str_fun_morph (A:Set){X Y:Set}: 
  (X->Y) -> MonStrFunObj A X -> MonStrFunObj A Y :=
  fun f s => fun_morph M (fun_morph (StrFun A) f) s.

Lemma Mon_str_fun_id_law: 
  forall (A:Set), FunctorIdLaw (MonStrFunObj A) (@mon_str_fun_morph A).
Proof.
intros A X.
apply functional_extensionality.
intro s.
unfold mon_str_fun_morph.
rewrite (fun_id_law (StrFun A)).
rewrite (fun_id_law M).
auto.
Qed.

Lemma mon_str_fun_comp_law: 
  forall (A:Set), FunctorCompLaw (MonStrFunObj A) (@mon_str_fun_morph A).
Proof.
intros A X Y Z f g.
apply functional_extensionality.
intros s.
unfold mon_str_fun_morph.
rewrite (fun_comp_law (StrFun A)).
rewrite (fun_comp_law M).
auto.
Qed.
