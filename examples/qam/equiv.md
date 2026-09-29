# Equivalence examples

Pairs under the Phase 2 policy `m1 ≡ m2 iff canon(normalize m1) =_struct
canon(normalize m2)`. Each row is locked by a test in `../../testCase.ml`.

## Equivalent (≡)

| Left | Right | Why |
|------|-------|-----|
| `{ 0, a & a }` | `{ a & a, 0 }` | soup is a multiset (order irrelevant) |
| `{ 0, a & b }` | `{ 0, b & a }` | meet commutativity |
| `{ nu c., c!x. }` | `{ 0, 0 }` | same normal form (COHERE) |
| `{ c->x., c?y. }` | `{ 0, 0 }` | same normal form (DECODE) |
| `{ a<-k., a }` | `{ 0, a & a }` | same normal form (ENCODE) |
| `{ 0, 0 }` | `{ 0, 0 }` | reflexive |
| `|[ { nu z., o }, phi, { c?d. } ]|` | (identical) | airlock structural compare |
| `{ repl nu c., o }` | `{ repl nu c., o }` | identical repl, same fuel |

## Not equivalent (≢)

| Left | Right | Why |
|------|-------|-----|
| `{ 0, a & a }` | `{ 0, 0 }` | distinct normal forms |
| `{ a!x. + b?y., o }` | `{ b?y. + a!x., o }` | Choice commits left ⇒ different NFs (documented MVP limit) |
| `|[ …, phi, … ]|` | `|[ …, psi, … ]|` | different airlock boundary resource |
| `{ repl nu c., o }` | `{ nu c., repl nu c., o }` | a repl term is not claimed equal to its one-step unfold |
