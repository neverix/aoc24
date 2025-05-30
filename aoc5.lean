open List
open Option


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

section mulp
  structure Parser (α : Type) where
    parse: (List Char -> Option (Prod α (List Char)))
  deriving Inhabited

  def single_char_parser (char: Char): Parser Char :=
    { parse := fun
      | List.nil => Option.none
      | List.cons c x => if c == char then Option.some (char, x) else Option.none
    }

  instance : Functor Parser where
    map := fun f p => {
      parse := fun l => p.parse l >>= (fun (res, s) => Option.some (f res, s))
    }

  instance : Pure Parser where
    pure := fun t => {
      parse := fun s => Option.some (t, s)
    }

  instance : Seq Parser where
    seq := fun mf mx => {
      parse := fun s => do
        let (res, rest) <- mf.parse s
        let (applied, rest) <- (mx Unit.unit).parse rest
        (res applied, rest)
    }

  def pa := single_char_parser 'A'
  def pb := single_char_parser 'B'
  #eval pa.parse "A".toList
  #eval pb.parse "B".toList
  def prepare_next (pa: Parser α) := ((fun c nxt => c :: nxt) <$> pa)
  def save_last (pb: Parser α): Parser (List α) := ((fun nxt => [nxt]) <$> pb)
  -- def u := (pa >> ((fun a b => b) <$> pb))

  def parseq: (parsers: List $ Parser α) -> Parser (List α) := fun
    | nil => pure []
    | cons a nil => save_last a
    | cons a x => (prepare_next a) <*> (parseq x)

  def string_parse (s: String): Parser String :=
    String.mk <$> (parseq (List.map single_char_parser s.toList))

  def abaca := string_parse "abaca"
  #eval abaca.parse "abaca".toList

  def me: Parser α := {
    parse := (fun _ => Option.none)
  }
  instance : Alternative Parser where
    failure := me
    orElse first second := {
      parse := fun string =>
        let first_result := first.parse string
        -- if first_result then first_result else
        match first_result with
          | some x => x
          | none =>
            let real_sec := (second Unit.unit)
            let second_result := real_sec.parse string
            second_result
    }

  def digits := "0123456789".toList
  def digitParser :=
    foldl (· <|> ·) me (map single_char_parser digits)

  #eval digitParser.parse "1".toList
  #eval digitParser.parse "".toList
  #eval digitParser.parse ",".toList
  #eval digitParser.parse "11,".toList

  partial def parse_fast
    (p: Parser α)
    (s: List Char)
    (epilogue:
      (Option (Prod (List α) (List Char)))
      -> (Option (Prod (List α) (List Char))))
      : (Option (Prod (List α) (List Char))) :=
    match p.parse s with
      | none => epilogue $ none
      | some (res, s) => parse_fast p s (fun
        | none => epilogue $ some ([res], s)
        | some (lst, remainder) =>
            epilogue $ some (res :: lst, remainder))

  partial def one_or_many (p: Parser α) [Inhabited α]: Parser (List α) :=
    {
      parse := (parse_fast p · id)
    }

  #eval (one_or_many digitParser).parse "55446".toList

  def numberParser := (String.toNat! $ String.mk ·) <$> (one_or_many digitParser)
  #eval numberParser.parse "012839 uwu".toList
  #eval numberParser.parse "e 012839 uwu".toList
  #eval numberParser.parse "56 uwu".toList

  inductive MulPart where
    | mulBr: String -> MulPart
    | num1: Nat -> MulPart
    | comma: String -> MulPart
    | num2: Nat -> MulPart
    | closeBr: String -> MulPart
  deriving Repr

  def mulParser := parseq [
    (MulPart.mulBr <$> string_parse "mul("),
    (MulPart.num1 <$> numberParser),
    (MulPart.comma <$> string_parse ","),
    (MulPart.num2 <$> numberParser),
    (MulPart.closeBr <$> string_parse ")")
  ]

  def choker: (Parser Unit) := {
    parse := fun
      | nil => none
      | cons _ x => some (Unit.unit, x)
  }

  inductive MulParsing where
    | mul: (List MulPart) -> MulParsing
    | choke: Unit -> MulParsing
  deriving Inhabited, Repr

  def longChoker := one_or_many choker
  #eval longChoker.parse "absefre".toList

  def baseParser :=
    ((MulParsing.mul <$> mulParser) <|> (MulParsing.choke <$> choker))
  -- #eval baseParser.parse "mul(2,3)".toList
  #eval baseParser.parse [' ', 'm', 'u', 'l', '(', '2', ',', '3', ')']
  def fullParser :=
    (one_or_many baseParser)
  def testStr := " mul(2,3) mul(2,3)c".toList
  -- def a := baseParser.parse testStr
  -- def b := match a with
  --   | none => none
  --   | some (x, rem) => some (x, baseParser.parse rem)
  -- #eval b
  #eval (parseq [baseParser, baseParser]).parse testStr
  #eval fullParser.parse testStr

  def computeString (s: String) :=
    let s := s.toList
    let parsed := fullParser.parse s
    match parsed with
      | none => 0
      | some (results, _) =>
        let result_nums :=
          List.map (fun
            | MulParsing.mul [
              MulPart.mulBr _,
              MulPart.num1 a,
              MulPart.comma _,
              MulPart.num2 b,
              MulPart.closeBr _] => a * b
            | _ => 0
          ) results
        List.foldl (· + ·) 0 result_nums

end mulp

def main : IO Unit := do
  let stdout ← IO.getStdout
  (IO.FS.withFile "input.txt" IO.FS.Mode.read) (fun stdin => do
    let text <- stdin.readToEnd
    stdout.print [computeString text]
  )
