import SciLean
open SciLean
open List
open Option
open SciLean.DataArrayN
open Std


def IO.FS.Stream.print (self: IO.FS.Stream) (xs: List α) [ToString α] :=
  self.putStrLn (
    String.join
    $ List.intersperse " "
    $ List.map toString xs)

def IO.FS.Handle.getNums (self: IO.FS.Handle) := self.getLine.map fun
  | "" => none
  | s =>
    let s := s.dropRightWhile Char.isWhitespace
    let n := s.splitOn.filter (!String.all · Char.isWhitespace)
    let i := n.map String.toNat!
    some i

partial def readAll (fn: IO (Option α)) := do
  let res <- fn
  match res with
    | none => return []
    | some x => do
      let remainder <- readAll fn
      return List.cons x remainder

#eval ⊞[1.0, 2.0]

def find_sums (a: Float^[n, m]) :=
  let b: Float^[n, m-3] := ⊞ i (j: Fin (m-3)) =>
    let x := a[i, (Fin.mk j (by omega))]
    let m := a[i, (Fin.mk (j+1) (by omega))]
    let a_ := a[i, (Fin.mk (j+2) (by omega))]
    let s := a[i, (Fin.mk (j+3) (by omega))]
    let b := (x == ('X'.toNat.toFloat)) && (m == ('M'.toNat.toFloat)) && (a_ == ('A'.toNat.toFloat)) && (s == ('S'.toNat.toFloat))
    b.toNat.toFloat
  b.sum

def SciLean.DataArrayN.mat_flip (A : Float^[I,J]) : Float^[I,J] := ⊞ (i: Fin I) (j: Fin J) =>
  -- let new_ind := I - i - 1
  -- A[Fin.mk new_ind (by omega), j]
  let new_ind := J - j - 1
  A[i,Fin.mk new_ind (by omega)]

-- def SciLean.DataArrayN.dia (A : Float^[I,J]) : Float^[(I + J - 1),J] := ⊞ (i: Fin (I + J - 1)) (j: Fin J) =>
--   if j.val > i.val then 0.0 else
--     if i.val - j.val >= I then 0.0 else
--       A[Fin.mk (i - j) (by sorry), j]

-- def SciLean.DataArrayN.dia (A : Float^[I,J]) :=
--   -- ⊞ (i: Fin I) (j: Fin (I + J + J)) =>
--   ⊞ (i: Fin I) (j: Fin (I + J + J)) =>
--     let val := i.val + j.val
--     if val < J then 0.0 else
--     let val := val - J
--     if val >= J then 0.0 else
--     A[i, Fin.mk (val) (by sorry)]
--     -- if j.val > i.val then 0.0 else
--     --   if i.val - j.val >= I then 0.0 else
--     --     A[Fin.mk (i - j) (by sorry), j]

-- def SciLean.DataArrayN.dia (A : Float^[I,J]) :=
--   ⊞ (i: Fin (I + J + I)) (j: Fin (J)) =>
--     let val := i.val + j.val
--     if val < I then 0.0 else
--     let val := val - I
--     if val >= I then 0.0 else
--     A[Fin.mk (val) (by sorry), j]

def bruh (x: Nat): Fin α := Fin.mk x (by sorry)

def SciLean.DataArrayN.diagg (A : Float^[I,J]) :=
  ⊞ (i: Fin (J + I - 1)) (j: Fin (J)) =>
    if i < J then
      let start_x := J - i - 1
      let start_y := 0
      let x := start_x + j
      let y := start_y + j.val
      if y >= I || x >= J then 0.0 else
      A[bruh y, bruh x]
    else
      let start_x := 0
      let start_y := i - J + 1
      let x := start_x + j.val
      let y := start_y + j
      if y >= I || x >= J then 0.0 else
      A[bruh y, bruh x]
    -- let val := i.val + j.val
    -- if val < I then 0.0 else
    -- let val := val - I
    -- if val >= I then 0.0 else
    -- A[Fin.mk (val) (by sorry), j]

-- #eval! ((⊞[1.0, 2.0, 3.0, 4.0, 5.0, 6.0]).reshape2 2 3 sorry).dia

-- partial def recurse_augment (a: Float^[n, m]) (k: Nat): List ((List Float) × Nat) :=
--   let (key, value, keyvals) := match (k.mod 4) with
--     | 0 =>
--       let u := a.mat_flip
--       (u.toList, find_sums u, match k with
--         | 0 => none
--         | n+1 => some $ (recurse_augment u n) ++ (recurse_augment a n))
--     | 1 =>
--       let u := a.transpose
--       (u.toList, find_sums u, match k with
--         | 0 => none
--         | n+1 => some $ (recurse_augment u n) ++ (recurse_augment a n))
--     | 2 =>
--       let u := a.dia
--       (u.toList, find_sums u, match k with
--         | 0 => none
--         | n+1 => some $ (recurse_augment u n) ++ (recurse_augment a n))
--     | _ =>
--       let u := a
--       (u.toList, find_sums u, match k with
--         | 0 => none
--         | n+1 => some $ (recurse_augment u n) ++ (recurse_augment a n))
--   let new_keyvals := match keyvals with
--     | none => []
--     | some x => x
--   let new_keyvals := if (new_keyvals.any (fun (k, v) => k == key)) then
--     new_keyvals
--   else (key, value.toUSize.toNat) :: new_keyvals
--   new_keyvals

def compute (text: String): List Float :=
    let lines: Array (Array Char) := (text.splitOn "\n").map (·.toList |> Array.mk) |> List.toArray
    let cols := (lines.get (Fin.mk 0 sorry) |> Array.size)
    if !(lines.all (·.size == cols)) then
      dbg_trace "Invalid input"
      []
    else
    let da: Float^[lines.size, cols] :=
      ArrayType.ofFn (fun x =>
        let r := (lines.get x.1)
        let c := (r.get (Fin.mk x.2.val sorry))
        c.toNat.toFloat
      )
    -- let u := recurse_augment da 8
    -- u.map (·.2.toFloat)
    -- [(List.foldl (fun acc (k, v) => acc + v) 0 u)]
    [
      (find_sums da),
      (find_sums da.mat_flip),
      (find_sums da.transpose),
      (find_sums da.transpose.mat_flip),
      (find_sums da.diagg),
      (find_sums da.diagg.mat_flip),
      (find_sums da.mat_flip.transpose.diagg),
      (find_sums da.mat_flip.transpose.diagg.mat_flip),
      -- (find_sums da.mat_flip.dia),
      -- (find_sums da.transpose.dia),
      -- (find_sums da.mat_flip.transpose.dia),
    ]

#eval! compute "xxxxxxx
XX X  U
 MM M U
  AA AU
   SSUS"
#eval! compute "xxxxxxx
.X .  U
 .M . U
  .A .U
   .SU."
#eval! compute "   XU
  M U
 AA U
S  SU"
#eval! compute "XMASXMASSAMXXMASXS
XMASXMASSMAXXMASMA
XMASXMASSAMXXMASAM
XMASXMASSAMXXMASSX"

#eval! compute "..X...
.SAMX.
.A..A.
XMAS.S
.X...."

#eval! (compute "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX").foldl (·+·) 0.0

def lemain :=
  (IO.FS.withFile "input.txt" IO.FS.Mode.read) (fun stdin => do
    let stdout ← IO.getStdout
    let text <- stdin.readToEnd
    let computations := ((compute text).map (·.toUSize.toNat))
    stdout.print computations
    let cmp := (foldl (·+·) 0 computations)
    stdout.print [cmp]
  )

-- #eval! lemain

def main : IO Unit := do
  _ <- lemain
  return Unit.unit
