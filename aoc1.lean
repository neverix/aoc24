def IO.FS.Stream.print (self: IO.FS.Stream) (xs: List α) [ToString α] :=
  self.putStrLn (
    String.join
    $ List.intersperse " "
    $ List.map toString xs)

def IO.FS.Handle.getNums (self: IO.FS.Handle) := self.getLine.map $ fun s =>
  let s := s.dropRightWhile Char.isWhitespace
  s.splitOn.filter (!String.all · Char.isWhitespace)

def main : IO Unit := do
  let stdout ← IO.getStdout
  (IO.FS.withFile "input.txt" IO.FS.Mode.read) (fun stdin => do
    let nums <- stdin.getNums
    stdout.print nums
  )
