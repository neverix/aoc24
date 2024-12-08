import Lake
open Lake DSL

package «mathtest» where
  -- Settings applied to both builds and interactive editing
  leanOptions := #[
    ⟨`pp.unicode.fun, true⟩ -- pretty-prints `fun a ↦ b`
  ]
  -- add any additional package configuration options here

require mathlib from git
  "https://github.com/leanprover-community/mathlib4.git"
require scilean from git
  "https://github.com/lecopivo/SciLean.git"

-- @[default_target]
-- lean_lib «Mathtest» {
--   --
-- }
  -- add any library configuration options here


-- @[default_target]
-- lean_exe guess {
--   root := `Mainn
-- }

@[default_target]
lean_exe aoc7 {
  root := `aoc7
}
