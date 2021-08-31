Require Export FunctionalExtensionality.

(* Definition of a container functor *)

Parameter Shape: Set.
Parameter Position: Shape -> Set.

Definition M (X:Set): Set := {s:Shape & Position s -> X}.

(* Functoriality of M *)

Definition mmap (A B:Set)(f:A->B): M A -> M B :=
fun x => match x with
  existT _ s h => existT _ s (fun p => f (h p))
end.

Definition id (X:Set): X -> X := fun x => x.

Definition comp (X Y Z:Set)(g:Y->Z)(f:X -> Y): X -> Z :=
fun x => g (f x).

Lemma m_functor_id:
  forall A, mmap A A (id A) = id (M A).
Proof.
intro A.
apply functional_extensionality.
intros [s h]; unfold id; simpl.
auto.
Qed.

Lemma m_functor_comp:
  forall (A B C:Set)(g:B->C)(f:A->B),
    mmap A C (comp A B C g f) = comp (M A) (M B) (M C) (mmap B C g) (mmap A B f).
Proof.
intros A B C g f.
apply functional_extensionality.
intros [s h].
auto.
Qed.

CoInductive MonStr (A:Set): Set :=
  mcons: M (A * (MonStr A)) -> MonStr A.

CoFixpoint monstr_map (A B : Set) (f : A -> B) (x : MonStr A) : MonStr B :=
  match x with
  | mcons _ (existT _ s h) =>
      mcons _ (existT _ s (fun p => (f (fst (h p)), monstr_map A B f x)))
  end.

Lemma monster_functor_id:
  forall A, monstr_map A A (id A) = id (MonStr A).
(* We should use bisimulation instead of equality,
   or postulate the principle of coinduction: bisimulation => equality
*)

