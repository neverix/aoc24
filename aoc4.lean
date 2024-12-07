
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
    let num_follow := List.foldl (fun x y => x + (
      let ascdesc := fun y =>
        let y := (y.zip y.tail!)
        List.foldl (fun (asc, desc) (prev, curr) =>
          let abs_diff := (Int.ofNat prev - curr).natAbs
          let abs_good := (abs_diff.blt 4).and ((0).blt abs_diff)
          let asc := asc.and $ (prev.ble curr).and abs_good
          let desc := desc.and $ (curr.ble prev).and abs_good
          (asc, desc)
        ) (true, true) y
      let (asc, desc) := ascdesc y
      let (_, _, asc_any, desc_any) := List.foldl (fun ((lst_before: List Nat), (lst_after: List Nat), asc_any, desc_any) elem => (
        let (a, d) := (ascdesc $ lst_before ++ lst_after)
        let asc_any := asc_any.or a
        let desc_any := desc_any.or d
        (lst_before ++ [elem], (match lst_after with
          | List.nil => []
          | List.cons _ ba => ba
        ), asc_any, desc_any)
      )) ([], y.tail!, false, false) y
      let (asc, desc) := (asc.or asc_any, desc.or desc_any)
      if asc.or desc then 1 else 0
    )) 0 nums

    stdout.print [num_follow]
  )
