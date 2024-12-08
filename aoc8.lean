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

def bruh (x: Nat): Fin α := Fin.mk x (by sorry)

def find_sums (a: Float^[n, m]) :=
  let b: Float^[n-2, m-2] := ⊞ (i: Fin (n - 2)) (j: Fin (m-2)) =>
    let (i, j) := (i.val, j.val)
    let m1 := a[bruh i, bruh j]
    let m2 := a[bruh i, bruh (j+2)]
    let a1 := a[bruh (i+1), bruh (j+1)]
    let s1 := a[bruh (i+2), bruh j]
    let s2 := a[bruh (i+2), bruh (j+2)]
    let b := (m1 == ('M'.toNat.toFloat)) && (m2 == ('M'.toNat.toFloat)) && (a1 == ('A'.toNat.toFloat)) && (s1 == ('S'.toNat.toFloat)) && (s2 == ('S'.toNat.toFloat))
    b.toNat.toFloat
  b.sum

def SciLean.DataArrayN.mat_flip (A : Float^[I,J]) : Float^[I,J] := ⊞ (i: Fin I) (j: Fin J) =>
  let new_ind := I - i - 1
  A[Fin.mk new_ind (by omega), j]


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
    [
      (find_sums da),
      (find_sums da.mat_flip),
      (find_sums da.mat_flip.transpose),
      (find_sums da.mat_flip.transpose.mat_flip),
    ]

#eval! compute ".....
.M.S.
..A..
.M.S.
....."
#eval! compute "M.S
.A.
M.S"
#eval! compute "S.S
.A.
M.M"
#eval! compute "S.M
.A.
S.M"
#eval! compute "M.M
.A.
S.S"
#eval! compute ".M.S......
..A..MSMS.
.M.S.MAA..
..A.ASMSM.
.M.S.M....
..........
S.S.S.S.S.
.A.A.A.A..
M.M.M.M.M.
.........."

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
