
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

partial def transpose { α: Type u } [ Inhabited α ]
  | List.cons [] (_: List $ List α) => []
  | [] => []
  | (x: List $ List α) => (x.map List.head!) :: (transpose <| x.map List.tail!)

section Sorter
  class BLT (α: Type 0) where
    blt: α → α → Bool

  instance Nat_blt: BLT Nat :=
    BLT.mk Nat.blt

  variable {α : Type 0}
  variable [Inhabited α] [BLT α]

  def swap (i: Nat) (j: Nat): StateM (Array α) Unit := modifyGet (Unit.unit, ·.swap! i j)

  def compare (i: Nat) (j: Nat): StateM (Array α) Bool := do
    let current <- get
    let (a, b) := (current.get! i, current.get! j)
    let c := BLT.blt a b
    return c


  def sorter: StateM (Array α) Unit := do
    let initial <- get
    let length := initial.size
    for i in (Array.range length) do
      for j in (List.iota i) do
        let jSmallerThanPredecessor <- compare j (j-1)
        if jSmallerThanPredecessor then
            swap j (j-1)
        -- if current < current then
        --   swap 0 1
        -- else
        --   swap 2 3
    return


  def List.sorted (self: List α) :=
    let vec := Array.mk self
    let (_, vec) := sorter.run vec
    vec.toList
end Sorter

def main : IO Unit := do
  let stdout ← IO.getStdout
  (IO.FS.withFile "input.txt" IO.FS.Mode.read) (fun stdin => do
    let nums <- readAll stdin.getNums
    let (a, b) <- match transpose nums with
      | [a, b] => pure (a, b)
      | _ => panic "Bad"
    let (a, b) := (a.sorted, b.sorted)
    let abses := List.map (fun (x, y) => (Int.ofNat x - y).natAbs) (a.zip b)
    let dist := List.foldl Nat.add 0 abses
    stdout.print [dist]
  )
