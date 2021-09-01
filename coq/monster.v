Require Export FunctionalExtensionality.

(* Definition of a container functor *)

Parameter Shape: Set.
Parameter Position: Shape -> Set.

Definition M (A:Set): Set := {s:Shape & Position s -> A}.

(* Lifting of a relation by M (used in def of bisimulation) *)

Inductive MRel {A:Set}(R:A->A->Prop): M A -> M A -> Prop :=
  mrlift: forall (s:Shape)(h1 h2: Position s -> A),
            (forall p: Position s, R (h1 p) (h2 p))
          -> MRel R (existT _ s h1) (existT _ s h2).

(* Functoriality of M *)

Definition mmap {A B:Set}(f:A->B): M A -> M B :=
fun x => match x with
  existT _ s h => existT _ s (fun p => f (h p))
end.

(* Identity and composition operators (are they not in the library?) *)

Definition id (A:Set): A -> A := fun x => x.

Definition comp {A B C:Set}(g:B->C)(f:A -> B): A -> C :=
fun x => g (f x).

(* M (any container) is a functor *)

Lemma m_functor_id:
  forall A, mmap (id A) = id (M A).
Proof.
intro A.
apply functional_extensionality.
intros [s h]; unfold id; simpl.
auto.
Qed.

Lemma m_functor_comp:
  forall (A B C:Set)(g:B->C)(f:A->B),
    mmap (comp g f) = comp (mmap g) (mmap f).
Proof.
intros A B C g f.
apply functional_extensionality.
intros [s h].
auto.
Qed.

(* Monadic streams over the functor M *)
CoInductive MonStr (A:Set): Set :=
  mcons: M (A * (MonStr A)) -> MonStr A.

Arguments mcons {A}.

Definition uncons {A:Set}: MonStr A -> M (A* MonStr A) :=
fun s => match s with (mcons m) => m end.

Lemma mcons_unfold:
  forall A (s:MonStr A), s = mcons (uncons s).
Proof.
intros A s; case s; auto.
Qed.

(* Lifting relations to a product *)
Definition prlift {A B: Set}(RA:A->A->Prop)(RB:B->B->Prop): A*B -> A*B -> Prop:=
fun p1 p2 => RA (fst p1) (fst p2) /\ RB (snd p1) (snd p2).

(* Bisimulation for monsters *)
CoInductive Mbisim {A:Set}: MonStr A -> MonStr A -> Prop :=
  mbisim: forall (m1 m2: M (A * (MonStr A))),
            MRel (prlift (eq) (@Mbisim A)) m1 m2
          -> Mbisim (mcons m1) (mcons m2). 

Notation "s1 ~~ s2" := (Mbisim s1 s2) (at level 70, no associativity).            

(* CoInduction Principle: bisimulation implies equality *)
Axiom coinduction: forall A (s1 s2: MonStr A),
                     s1 ~~ s2 -> s1 = s2.

(* Pairing of functions *)
Definition fpair {A1 A2 B1 B2:Set}(f:A1->A2)(g:B1->B2): A1*B1 -> A2*B2 :=
fun p => pair (f (fst p)) (g (snd p)).

CoFixpoint monstr_map {A B : Set}(f : A -> B) (x : MonStr A) : MonStr B :=
  match x with
    mcons m => mcons (mmap (fpair f (monstr_map f)) m)
  end.

(*
  match x with
  | mcons _ (existT _ s h) =>
      mcons _ (existT _ s (fun p => (f (fst (h p)), monstr_map A B f x)))
  end.
*)

Lemma mmap_unfold: 
  forall (A B : Set) (f : A -> B)(m: M (A * MonStr A)),
    monstr_map f (mcons m) = mcons (mmap (fpair f (monstr_map f)) m).
Proof.
intros A B f m.
symmetry.
rewrite mcons_unfold.
auto.
Qed.


(* MonStr is a functor *)

Lemma monster_id_law:
  forall A s, monstr_map (id A) s ~~ id (MonStr A) s.
Proof.
cofix mil.
intros A [m].
unfold id; simpl.
rewrite mmap_unfold; simpl.
apply mbisim.
case m. 
intros s h.
unfold mmap; simpl.
apply mrlift.
intro p.
unfold prlift; simpl; split.
auto.
apply mil.
Qed.

Lemma monster_functor_id:
  forall A, monstr_map (id A) = id (MonStr A).
Proof.
intro A; apply functional_extensionality.
intro s; apply coinduction.
apply monster_id_law.
Qed.
