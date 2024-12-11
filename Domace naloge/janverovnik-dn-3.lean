-- set_option autoImplicit false

/------------------------------------------------------------------------------
 ## Naravna števila

 Definirajte funkcijo, ki _rekurzivno_ (torej naivno in ne direktno s formulo,
 ki jo boste morali dokazati) sešteje prvih `n` naravnih števil, ter
 dokažite, da zanjo velja znana enakost (najprej v obliki, ki ne zahteva
 deljenja, nato pa še v običajni obliki).
------------------------------------------------------------------------------/

-- def aux : Nat → Nat → Nat :=
--   fun n => fun acc =>
--     match n, acc with
--     | 0, acc => acc
--     | m + 1, acc => aux m (acc + n)

-- def vsota_prvih : Nat → Nat := fun n => aux n 0

def vsota_prvih : Nat → Nat := --upam da ni treba repno rekurzivne
  fun n =>
    match n with
    | Nat.zero => 0
    | Nat.succ m => n + vsota_prvih m

#eval vsota_prvih 100

theorem gauss : (n : Nat) → 2 * vsota_prvih n = n * (n + 1) :=
  by
    intro n
    induction n with
    | zero => simp [vsota_prvih]
    | succ m ip => sorry

theorem cisto_pravi_gauss : (n : Nat) → vsota_prvih n = (n * (n + 1)) / 2 :=
  by
    intro n
    induction n with
    | zero => simp[vsota_prvih]
    | succ m ip => sorry


/------------------------------------------------------------------------------
 ## Vektorji

 Definirajmo vektorje podobno kot na predavanjih, le da namesto svojih naravnih
 števil uporabimo vgrajena. Da se tipi ujamejo, funkcijo stikanja napišemo s
 pomočjo taktik.

 Napišite funkcijo `obrni`, ki vrne na glavo obrnjen vektor, ter funkciji
 `glava` in `rep`, ki varno vrneta glavo in rep _nepraznega_ seznama.
------------------------------------------------------------------------------/

inductive Vektor : Type → Nat → Type where
  | prazen : {A : Type} → Vektor A 0
  | sestavljen : {A : Type} → {n : Nat} → A → Vektor A n → Vektor A (n + 1)
deriving Repr

def stakni : {A : Type} → {m n : Nat} → Vektor A m → Vektor A n → Vektor A (m + n) :=
  fun xs ys => match xs with
  | .prazen => by rw [Nat.add_comm]; exact ys
  | .sestavljen x xs' => by rw [Nat.add_right_comm]; exact Vektor.sestavljen x (stakni xs' ys)

def obrni : {A : Type} → {n : Nat} → Vektor A n → Vektor A n :=
  sorry

def glava : sorry :=
  sorry

def rep : sorry :=
  sorry

/------------------------------------------------------------------------------
 ## Predikatni račun

 Dokažite spodnje tri trditve. Zadnja je _paradoks pivca_, ki pravi:
   "V vsaki neprazni gostilni obstaja gost, za katerega velja,
   da če pije on, pijejo vsi v gostilni."
 Za dokaz potrebujete klasično logiko, torej nekaj iz modula `Classical`.
------------------------------------------------------------------------------/

theorem forall_implies : {A : Type} → {P Q : A → Prop} →
  (∀ x, (P x → Q x)) → (∀ x, P x) → (∀ x, Q x) := by
  sorry

theorem forall_implies' : {A : Type} → {P : Prop} → {Q : A → Prop} →
  (∀ x, (P → Q x)) ↔ (P → ∀ x, Q x) := by
  sorry

theorem paradoks_pivca :
  {G : Type} → {P : G → Prop} →
  (g : G) →  -- (g : G) pove, da je v gostilni vsaj en gost
  ∃ (p : G), (P p → ∀ (x : G), P x) := by
  sorry

/------------------------------------------------------------------------------
 ## Dvojiška drevesa

 Podan naj bo tip dvojiških dreves skupaj s funkcijama za zrcaljenje in izračun
 višine ter dvema funkcijama, ki obe od leve proti desni naštejeta elemente
 drevesa. Pri tem prva deluje naivno in ima časovno zahtevnost O(n log n), druga
 pa je malo bolj zapletena in deluje v času O(n). Dokažite spodnje enakosti, pri
 čemer lahko do pomožne funkcije `aux` dostopate kot `elementi'.aux`
-------------------------------------------------------------------------------/

inductive Drevo : Type → Type where
  | prazno : {A : Type} → Drevo A
  | sestavljeno : {A : Type} → Drevo A → A → Drevo A → Drevo A

def zrcali : {A : Type} → Drevo A → Drevo A :=
  fun t => match t with
  | .prazno => .prazno
  | .sestavljeno l x d => .sestavljeno (zrcali d) x (zrcali l)

def visina : {A : Type} → Drevo A → Nat :=
  fun t => match t with
  | .prazno => 0
  | .sestavljeno l _ d => 1 + max (visina l) (visina d)

def elementi : {A : Type} → Drevo A → List A :=
  fun t => match t with
  | .prazno => []
  | .sestavljeno l x d => elementi l ++ x :: elementi d

def elementi' : {A : Type} → Drevo A → List A :=
  let rec aux : {A : Type} → Drevo A → List A → List A :=
    fun t acc => match t with
    | .prazno => acc
    | .sestavljeno l x d => aux l (x :: aux d acc)
  fun t => aux t []

theorem zrcali_zrcali : {A : Type} → (t : Drevo A) → zrcali (zrcali t) = t :=
  by
    intros A t
    induction t with
      | prazno => simp [zrcali]
      | sestavljeno l x d ipl ipd =>
      simp[zrcali]
   -- trivial
      constructor
      exact ipl
      exact ipd

theorem visina_zrcali : {A : Type} → (t : Drevo A) → visina (zrcali t) = visina t :=
  by
    intros A t
    induction t with
    | prazno => simp[zrcali]
    | sestavljeno l x d ipl ipd =>
      simp [zrcali, visina, ipl, ipd, Nat.max_comm]

theorem elementi_elementi'.aux {A : Type} : ∀ {t : Drevo A} {acc : List A}, elementi t ++ acc = elementi'.aux t acc :=
  by
    intro t
    induction t with
    | prazno => simp [elementi, elementi'.aux]
    | sestavljeno l x d ipl ipd =>
      intro acc
      simp [elementi, elementi'.aux, ipd]
      rw[ipl]

theorem elementi_elementi' : {A : Type} → (t : Drevo A) → elementi t = elementi' t :=
  by
    intros A t
    induction t with
    | prazno => simp [elementi, elementi', elementi'.aux]
    | sestavljeno l x d =>
       simp [elementi, elementi']
       rw [<- elementi_elementi'.aux]
       simp[elementi]
