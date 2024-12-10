open List
open Option
open Std


def IO.FS.Stream.print (self: IO.FS.Stream) (xs: List α) [ToString α] :=
  self.putStrLn (
    String.join
    $ List.intersperse " "
    $ List.map toString xs)

-- def IO.FS.Handle.getNums (self: IO.FS.Handle) := self.getLine.map fun
--   | "" => none
--   | s =>
--     let s := s.dropRightWhile Char.isWhitespace
--     let n := s.splitOn.filter (!String.all · Char.isWhitespace)
--     let i := n.map String.toNat!
--     some i

def IO.FS.Handle.splitNums (s: String) :=
    let s := s.dropRightWhile Char.isWhitespace
    let n := s.splitOn.filter (!String.all · Char.isWhitespace)
    let i := n.map String.toNat!
    i

partial def readAll (fn: IO (Option α)) := do
  let res <- fn
  match res with
    | none => return []
    | some x => do
      let remainder <- readAll fn
      return List.cons x remainder

inductive Tern where
  | add : Tern
  | mul : Tern
  | bar : Tern

def combinationAt (n: Nat) (i: Nat): List Tern :=
  (range n)
    |> map (
      fun x =>
        let divided := (x / ((3).pow i)).mod 3
        match divided with
          | 0 => Tern.add
          | 1 => Tern.mul
          | _ => Tern.bar
      )

def plusSum (xs: List Nat) (i: Nat): Nat :=
  if length xs < 1 then 0 else
  let ps := combinationAt (xs.length - 1) i
  let (total, _) := foldl (fun (curVal, operation) nxt =>
    (if operation.head! then curVal + nxt else curVal * nxt, operation.tail!)
  ) (head! xs, ps) xs.tail!
  total

#check ((5).lt (6))
#eval (5 &&& (1 <<< 2)) > 0
#eval booleanAt 5 9
#eval plusSum [81, 40, 27] 1

def lemain :=
  (IO.FS.withFile "input.txt" IO.FS.Mode.read) (fun stdin => do
    let stdout ← IO.getStdout
    let text <- stdin.readToEnd
    let lines := text.splitOn "\n"
    let parsed := lines
      |> map (fun line => match line.splitOn ": " with
        | [a, b] => (a.toNat!, b.splitOn |> map String.toNat!)
        | _ => panic "parse error"
      )
    let total := foldl (fun cur_sum (target, vals) =>
      cur_sum + target * (Bool.toNat (
        any
        (range ((3).pow (length vals - 1)))
        (target == plusSum vals ·)))
    ) 0 parsed
    stdout.print [total]
  )

-- #eval! lemain

def main : IO Unit := do
  _ <- lemain
  return Unit.unit
