
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
      let ascdesc := fun y => List.foldl (fun (asc, desc, ast, dst) ((prev, curr), next) =>
        let check_asc := fun asc desc prev (curr: Nat) =>
          let abs_diff := (Int.ofNat prev - curr).natAbs
          let abs_good := (abs_diff.blt 4).and ((0).blt abs_diff)
          let asc := asc.and $ (prev.ble curr).and abs_good
          let desc := desc.and $ (curr.ble prev).and abs_good
          (asc, desc)
        let (an, dn) := check_asc asc desc prev curr
        let checker := fun (cond_past: Bool) (cond: Bool) (cond_next: Bool) (trig: Nat) =>
          if not cond_past then (false, trig) else
            if (0).blt trig then (cond_past.and cond, trig) else
              if cond then (true, trig) else
                if cond_next then (true, 1) else
                  (false, trig)
        let (asc, desc, ast, dst) := match next with
          | some next =>
            let (a, d) := check_asc asc desc prev next
            dbg_trace "{(an, a, dn, d)}"
            let (asc, ast) := checker asc an a ast
            let (desc, dst) := checker desc dn d dst
            (asc, desc, ast, dst)
          | none => (an, dn, ast, dst)
        dbg_trace "{(asc, desc, ast, dst)}"
        (asc, desc, ast, dst)
      ) (true, true, 0, 0) y
      dbg_trace "idk"
      let (asc, desc, ast, dst) := ascdesc ((y.zip y.tail!).zip ((y.tail!.map Option.some) ++ [Option.none]))
      if asc.or desc then 1 else 0
    )) 0 nums

    stdout.print [num_follow]
  )
