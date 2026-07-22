# QAM reduction examples

Each `.qam` file holds one membrane. The `.qam` surface syntax has no comment
form, so expected normal forms are documented here (and asserted in
`../../testCase.ml`). Normal forms are printed by `Interpreter.string_of_membrane`.

| File | Input | Rule exercised | Expected normal form |
|------|-------|----------------|----------------------|
| `encode.qam` | `{ a<-k., a }` | ENCODE | `{ 0, a & a }` |
| `cohere.qam` | `{ nu c., c!x. }` | COHERE | `{ 0, 0 }` |
| `decode.qam` | `{ c->x., c?y. }` | DECODE | `{ 0, 0 }` |

Notes:
- `0` is `NullProcess`; `a & a` is `MeetOperation (SimpleResource "a", SimpleResource "a")`,
  the resource ENCODE produces by meeting the target resource with the encode channel.
- All three normal forms parse back through the grammar (round-trip), which is
  why ENCODE's `&` output needed surface syntax.
