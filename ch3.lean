variable (p q r : Prop)

-- commutativity of ∧ and ∨
example : p ∧ q ↔ q ∧ p :=
  Iff.intro
    (fun h: And p q => And.intro h.right h.left)
    (fun h: And q p => And.intro h.right h.left)
example : p ∨ q ↔ q ∨ p :=
  Iff.intro
    (fun h: Or p q => Or.elim h
      (fun hp: p => Or.intro_right q hp)
      (fun hq: q => Or.intro_left p hq))
    (fun h: Or q p => Or.elim h
      (fun hq: q => Or.intro_right p hq)
      (fun hp: p => Or.intro_left q hp))


-- associativity of ∧ and ∨
example : (p ∧ q) ∧ r ↔ p ∧ (q ∧ r) :=
  Iff.intro
    (fun h: (p ∧ q) ∧ r => And.intro
      h.left.left
      (And.intro h.left.right h.right))
    (fun h: p ∧ (q ∧ r) => And.intro
      (And.intro h.left h.right.left)
      h.right.right)
example : (p ∨ q) ∨ r ↔ p ∨ (q ∨ r) := Iff.intro
  (fun pqr => pqr.elim
    (fun pq => pq.elim
      (fun p => Or.intro_left (q ∨ r) p)
      fun q => Or.intro_right p (Or.intro_left r q))
    fun r => Or.intro_right p (Or.intro_right q r))
  (fun pqr => pqr.elim
    (fun p => Or.intro_left r (Or.intro_left q p))
    fun qr => qr.elim
      (fun q => Or.intro_left r (Or.intro_right p q))
      fun r => Or.intro_right (p ∨ q) r)

-- distributivity
example : p ∧ (q ∨ r) ↔ (p ∧ q) ∨ (p ∧ r) := Iff.intro
  (fun paqur =>
    have hp := paqur.left
    have qur := paqur.right
    qur.elim
      (fun hq => Or.intro_left _ $ And.intro hp hq)
      (fun hr => Or.intro_right _ $ And.intro hp hr)
    )
  (fun paqupar => paqupar.elim
    (fun paq => And.intro paq.left (Or.intro_left _ paq.right))
    (fun par => And.intro par.left (Or.intro_right _ par.right)))
example : p ∨ (q ∧ r) ↔ (p ∨ q) ∧ (p ∨ r) := Iff.intro
  (fun puqr => puqr.elim
    (fun p => And.intro (Or.intro_left q p) (Or.intro_left r p))
    (fun qr => And.intro (Or.intro_right p qr.left) (Or.intro_right p qr.right)))
  (fun puqpur =>
    have puq := puqpur.left
    have pur := puqpur.right
    puq.elim
      (fun p => Or.intro_left (q ∧ r) p)
      fun hq => pur.elim
        (fun p => Or.intro_left (q ∧ r) p)
        fun hr => Or.intro_right p $ And.intro hq hr
  )

-- other properties
example : (p → (q → r)) ↔ (p ∧ q → r) := Iff.intro
  (fun pqr => fun paq => pqr paq.left paq.right)
  (fun paqr => fun p => fun q => paqr $ And.intro p q)
example : ((p ∨ q) → r) ↔ (p → r) ∧ (q → r) := Iff.intro
  (fun puqr => And.intro
    (fun p => puqr (Or.intro_left _ p))
    (fun q => puqr (Or.intro_right _ q)))
  (fun prqr => fun puq => puq.elim
    (fun p => prqr.left p)
    (fun q => prqr.right q))
example : ¬(p ∨ q) ↔ ¬p ∧ ¬q :=
  Iff.intro
    (fun outer: ¬(p ∨ q) => ⟨
      (fun hp: p => outer (Or.intro_left q hp)),
      (fun hq: q => outer (Or.intro_right p hq))
    ⟩)
    (fun inner: (¬p ∧ ¬q) =>
      (fun poq: p ∨ q => poq.elim
        (fun hp: p => inner.left hp)
        (fun hq: q => inner.right hq)))
example : ¬p ∨ ¬q → ¬(p ∧ q) := fun npnq => npnq.elim
  (fun np => fun paq => np paq.left)
  (fun nq => fun paq => nq paq.right)
example : ¬(p ∧ ¬p) := fun hpnp: p ∧ ¬p =>
  False.elim (hpnp.right hpnp.left)
example : p ∧ ¬q → ¬(p → q) := fun panq pq =>
  panq.right (pq panq.left)
example : ¬p → (p → q) := fun np p => False.elim (np p)
example : (¬p ∨ q) → (p → q) := fun hpaq: ¬p ∨ q =>
  fun hp: p => hpaq.elim
    (fun hnp: ¬p => False.elim (hnp hp))
    id
example : p ∨ False ↔ p :=
  suffices
    hyp:
    (p ∨ False -> p)
    ∧ (p -> p ∨ False)
  from Iff.intro hyp.left hyp.right
  show ((p ∨ False) -> p) ∧ (p -> (p ∨ False)) from ⟨
    (fun pf: p ∨ False => pf.resolve_right id),
    (fun hp: p => Or.intro_left False hp)⟩
example : p ∧ False ↔ False := Iff.intro
  (fun paf => False.elim paf.right)
  (fun f => False.elim f)
example : (p → q) → (¬q → ¬p) := fun pq =>
  fun notq => fun p => notq $ pq p
