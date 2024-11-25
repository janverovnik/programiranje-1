
variable (α : Type) (p q : α → Prop) (r : Prop)
variable (r : Prop)

-- Izjave napišite na list papirja, nato pa jih dokažite v datoteki.

example : (¬ ∃ x, p x) ↔ (∀ x, ¬ p x) :=
  by
    apply Iff.intro
    intro h
    rw[not_exists] at h
    exact h
    intro h
    rw[not_exists]
    exact h

example : (r → ∀ x, p x) ↔ (∀ x, r → p x) :=
  sorry

example : r ∧ (∃ x, p x) ↔ (∃ x, r ∧ p x) := by
  sorry

example : r ∨ (∀ x, p x) → (∀ x, r ∨ p x) :=
  sorry

-- Tu pa nam bo v pomoč klasična logika
-- namig: `Classical.byContradiction` in `Classical.em` sta lahko v pomoč
open Classical

example : (¬ ∀ x, p x) ↔ (∃ x, ¬ p x) :=
 sorry

example : r ∨ (∀ x, p x) ↔ (∀ x, r ∨ p x) :=
  sorry
