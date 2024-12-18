def concat {A : Type} : List A → List A → List A :=
  fun xs ys =>
    match xs with
    | [] => ys
    | x :: xs' => x :: concat xs' ys

#check (concat ["a", "b"] ["c", "d"])

def reverse {A : Type} : List A → List A :=
  fun xs =>
  match xs with
  | [] => []
  | x :: xs' => concat (reverse xs') [x]

#check (reverse ["a", "b", "c", "d"])
#eval (reverse ["a", "b", "c", "d"])

def length {A : Type} : List A → Nat :=
  fun xs =>
  match xs with
  | [] => 0
  | _ :: xs' => 1 + length xs'

#check (length ["a", "b", "c", "d"])
#eval (length ["a", "b", "c", "d"])

theorem trd1 {A : Type} {x : A} : reverse [x] = [x] :=
  by
    simp [reverse, concat]

theorem trd2 {A : Type} {xs ys : List A} : length (concat xs ys) = length xs + length ys :=
  by
    induction xs with
    | nil => simp[concat, length]
    | cons x xs' ip =>
      simp[concat, length]
      rw[ip, Nat.add_assoc]

-- Tega poznamo že iz predavanj
theorem trd3 {A : Type} {xs : List A} : concat xs [] = xs :=
  by
    induction xs with
    | nil =>
      simp [concat]
    | cons x xs' ih =>
      simp [concat]
      rw [ih]

theorem trd4 {A : Type} {xs ys zs : List A} : concat (concat xs ys) zs = concat xs (concat ys zs) :=
  by
    induction xs with
    | nil => simp[concat]
    | cons x xs' ip =>
      simp[concat]
      rw[ip]

theorem trd5 {A : Type} {xs ys : List A} : reverse (concat xs ys) = concat (reverse ys) (reverse xs) :=
  by
    induction xs with
    | nil =>
    simp[reverse, concat]
    rw[trd3]
    | cons x xs' ip =>
    simp[reverse, concat]
    rw[ip, trd4]


theorem trd6 {A : Type} {xs : List A} : length (reverse xs) = length xs :=
  by
    induction xs with
    | nil => simp[length]
    | cons x xs' ip =>
      simp[reverse, length, concat]
      rw[trd2]
      simp[length]
      rw[ip, Nat.add_comm]


theorem trd7 {A : Type} {xs : List A} : reverse (reverse xs) = xs :=
  by
    induction xs with
    | nil => simp[reverse]
    | cons x xs' ip =>
    simp[reverse]
    rw[trd5, ip, trd1]
    simp[concat]



def map {A B : Type} : (A → B) → List A → List B :=
  fun f xs => match xs with
  | [] => []
  | x :: xs' => (f x) :: (map f xs')


theorem map_assoc {A B C : Type} {f : A → B} {g : B → C} {xs : List A} : map g (map f xs) = map (g ∘ f) xs :=
  sorry

theorem map_id {A : Type} {xs : List A} : map id xs = xs :=
  sorry

theorem map_concat {A B : Type} {f : A → B} {xs ys : List A} : map f (concat xs ys) = concat (map f xs) (map f ys) :=
  sorry


theorem map_reverse {A B : Type} {f : A → B} {xs : List A} : map f (reverse xs) = reverse (map f xs) :=
  sorry

inductive tree (A : Type) : Type where
  | empty : tree A
  | node : A → tree A → tree A → tree A

#check tree.rec

def tree_map {A B : Type} : (A → B) → tree A → tree B :=
  sorry

theorem tree_map_empty {A B : Type} {f : A → B} : tree_map f tree.empty = tree.empty :=
  sorry

theorem tree_map_comp {A B C : Type} {f : A → B} {g : B → C} {t : tree A} : tree_map g (tree_map f t) = tree_map (g ∘ f) t :=
  sorry

def depth {A : Type} : tree A → Nat :=
  fun t =>
    match t with
    | tree.empty => 0
    | tree.node _ l r => 1 + Nat.max (depth l) (depth r)

-- S tem se ne bomo ukvarjali
theorem max_comm {a b : Nat} : Nat.max a b = Nat.max b a :=
  sorry

def mirror {A : Type} : tree A → tree A :=
  fun t =>
    match t with
    | tree.empty => tree.empty
    | tree.node x l r => tree.node x (mirror l) (mirror r)

theorem mirror_depth {A : Type} {t : tree A} : depth (mirror t) = depth t :=
  by
    induction t with
    | empty => simp[depth]
    | node x l r hl hr =>
      simp[depth]
      rw[hl, hr]

theorem mirror_mirror {A : Type} {t : tree A} : mirror (mirror t) = t :=
  by
    induction t with
    | empty => simp[mirror]
    | node x l r hl hr =>
    simp[mirror]
    constructor
    exact hl
    exact hr

def collect {A : Type} : tree A → List A :=
  fun t =>
    match t with
    | tree.empty => []
    | tree.node x l r => concat (collect l) (concat [x]  (collect r))

theorem trd8 {A : Type} {x : A} {xs ys : List A} : concat xs (x :: ys) = concat (concat xs [x]) ys :=
   by
    induction xs with
    | nil => simp [concat]
    | cons x xs' ip =>
    simp [concat]
    assumption

theorem collect_mirror {A : Type} {t : tree A} : collect (mirror t) = reverse (collect t) :=
  sorry


def size {A : Type} : tree A → Nat :=
  sorry

theorem size_mirror {A : Type} {t : tree A} : size (mirror t) = size t :=
  sorry


--- Indukcija na pomožnih funkcijah z akumulatorjem

theorem concat2 : concat xs (x :: ys) = concat (concat (xs) [x]) ys :=
  by
    induction xs with
    | nil => simp [concat]
    | cons x xs' ip =>
    simp [concat]
    assumption

-- Definirajte repno rekurzivno funkcijo, ki obrne seznam

def reverse' {A : Type} (xs : List A) : List A :=
  let rec aux : List A → List A → List A
    | [], acc => acc
    | x :: xs', acc => aux xs' (x :: acc)
  aux xs []

def reverse'' {A : Type} : List A → List A :=
  fun xs =>
    match xs with
    | [] => []
    | x :: xs' => (reverse'' xs') ++ [x]

-- Dokažite, da je vaša funkcija pravilna

theorem reverse''_eq_reverse'.aux {A : Type} : ∀ {xs acc : List A}, (reverse'' xs) ++ acc = reverse'.aux xs acc :=
  by
    intro xs
    induction xs with
    | nil =>
      simp [reverse'', reverse'.aux]
    | cons x xs' ip =>
      intro acc
      simp [reverse'', reverse'.aux]
      rw[ip]


theorem reverse''_eq_reverse' {A : Type} : ∀ {xs : List A}, reverse'' xs = reverse' xs :=
  by
    intro xs
    induction xs with
    | nil => simp [reverse', reverse'', reverse'.aux]
    | cons x xs' =>
      simp [reverse', reverse'', reverse'.aux]
      apply reverse''_eq_reverse'.aux
