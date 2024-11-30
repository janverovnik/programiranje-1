-- Izomorfizmi

theorem eq1 {A B : Prop} : (A ∧ B) ↔ (B ∧ A) :=
  by
    apply Iff.intro
    intro h
    symm at h
    exact h
    intro h
    symm at h
    exact h

theorem eq2 {A B : Prop} : (A ∨ B) ↔ (B ∨ A) :=
  by
    apply Iff.intro
    intro h
    symm at h
    exact h
    intro h
    symm at h
    exact h

theorem eq3 {A B C : Prop} : (A ∧ (B ∧ C)) ↔ (B ∧ (A ∧ C)) :=
  by
    constructor
    intro h
    constructor
    have h2 := And.left (And.right h)
    exact h2

    have h1 := And.right (And.right h)
    have h2 := And.left h
    constructor
    exact h2
    exact h1

    intro h
    constructor
    have h1 := And.left (And.right h)
    exact h1

    have h1 := And.left h
    have h2 := And.right (And.right h)
    constructor
    exact h1
    exact h2

theorem eq4 {A B C : Prop} : (A ∨ (B ∨ C)) ↔ (B ∨ (A ∨ C)) :=
  sorry


theorem eq5 {A B C : Prop} : A ∧ (B ∨ C) ↔ (A ∧ B) ∨ (A ∧ C) :=
  by
    apply Iff.intro
    intro h
    rw[and_or_left] at h
    exact h
    intro h
    rw[and_or_left]
    exact h

theorem eq6 {A B C : Prop} : (B ∨ C) → A ↔ (B → A) ∧ (C → A) :=
  by
    apply Iff.intro
    intro h
    rw[or_imp] at h
    exact h
    intro h
    rw[or_imp]
    exact h

theorem eq7 {A B C : Prop} : C → (A ∧ B) ↔ (C → A) ∧ (C → B) :=
  by
    apply Iff.intro
    intro h
    rw[imp_and] at h
    exact h
    intro h
    rw[imp_and]
    exact h
