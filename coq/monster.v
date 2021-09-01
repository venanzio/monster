Require Export FunctionalExtensionality.

(* Definition of a container functor *)

Parameter Shape: Set.
Parameter Position: Shape -> Set.

Definition M (X:Set): Set := {s:Shape & Position s -> X}.

(* Lifting of a relation by M (used in def of bisimulation) *)

Inductive MRel (A:Set)(R:A->A->Prop): M A -> M A -> Prop :=
  mrlift: forall (s:Shape)(h1 h2: Position s -> A),
            (forall p: Position s, R (h1 p) (h2 p))
          -> MRel A R (existT _ s h1) (existT _ s h2).

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

(* Monadic streams over the functor M *)
CoInductive MonStr (A:Set): Set :=
  mcons: M (A * (MonStr A)) -> MonStr A.

Definition uncons (A:Set): MonStr A -> M (A* MonStr A) :=
fun s => match s with (mcons _ m) => m end.

Lemma mcons_unfold:
  forall A (s:MonStr A), s = mcons _ (uncons _ s).
Proof.
intros A s; case s; auto.
Qed.

(* Lifting relations to a product *)
Definition prlift (A B: Set)(RA:A->A->Prop)(RB:B->B->Prop): A*B -> A*B -> Prop:=
fun p1 p2 => RA (fst p1) (fst p2) /\ RB (snd p1) (snd p2).

(* Bisimulation for monsters *)
CoInductive MonStrEq (A:Set): MonStr A -> MonStr A -> Prop :=
  mbisim: forall (m1 m2: M (A * (MonStr A))),
            MRel _ (prlift _ _ (eq) (MonStrEq A)) m1 m2
          -> MonStrEq A (mcons _ m1) (mcons _ m2). 
            
(* CoInduction Principle: bisimulation implies equality *)
Axiom coinduction: forall A (s1 s2: MonStr A),
                     MonStrEq _ s1 s2 -> s1 = s2.

(* Pairing of functions *)
Definition fpair (A1 A2 B1 B2:Set)(f:A1->A2)(g:B1->B2): A1*B1 -> A2*B2 :=
fun p => match p with pair a b => pair (f a) (g b) end.

CoFixpoint monstr_map (A B : Set) (f : A -> B) (x : MonStr A) : MonStr B :=
  match x with
    mcons _ m => mcons _ (mmap _ _ (fpair _ _ _ _ f (monstr_map A B f)) m)
  end.

(*
  match x with
  | mcons _ (existT _ s h) =>
      mcons _ (existT _ s (fun p => (f (fst (h p)), monstr_map A B f x)))
  end.
*)

Lemma mmap_unfold: 
  forall (A B : Set) (f : A -> B)(m: M (A * MonStr A)),
    monstr_map A B f (mcons _ m) 
    = mcons _ (mmap _ _ (fpair _ _ _ _ f (monstr_map A B f)) m).
Proof.
intros A B f m.
symmetry.
rewrite mcons_unfold.
auto.
Qed.






Lemma monster_functor_id:
  forall A, monstr_map A A (id A) = id (MonStr A).
Proof.
intro A; apply functional_extensionality.
intros [m].
apply coinduction.
rewrite mmap_unfold.
(* To be completed *)
