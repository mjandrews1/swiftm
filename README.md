# SwiftM — spec-first M (MUMPS) in Swift for macOS

SwiftM is an independent implementation of M (ISO/IEC 11756) written in Swift,
built **spec-first**: every semantics lands with a Dafny proof in
[`mjandrews1/m-semantics`](https://github.com/mjandrews1/m-semantics) (the
`MSemantics` / `MControl` / `MRelOps` / `MSetList` / `MCommand` corpus) before
or alongside the Swift code, so the proofs are validated against a second,
independent implementation. Tracked at
[m-semantics#72](https://github.com/mjandrews1/m-semantics/issues/72).

Scope guard (from the m-semantics#72 decision): a **minimal-conforming M**
subset of the 139-case shared corpus passes on **swiftm AND mjvm AND GT.M** —
see `docs/MINIMAL-CONFORMING-M.md` in m-semantics for the 124-case gate
(116 ISO-core + 8 ISO-1999; 15 vendor cases are compatibility, not gate).
Written against ISO 11756 first, Dafny as cross-check. macOS-only (the fleet is
Linux). **NOT** a replacement for mjvm/JavaM — no full VistA surface (the 21k
swallowed-line cases are mjvm's job). First real workload: a portable
concurrency app (JOB/LOCK/$TEST/TSTART), actor-based.

## Layout (SPM)

```
Sources/
  parser/    M source text -> AST (line structure, commands, postconditionals,
             comma lists — semantics pinned in MSetList.dfy / MCommand.dfy)
  IR/        AST -> IR (ops mirroring mjvm's IR; control flow per MControl.dfy)
  runtime/   IR interpreter + MValue (value = string form, isNum predicate per
             MSemantics.dfy), store, $TEST, traps, actor-based JOB/LOCK
  storage/   persistent store (tieredstore-compatible design, TieredStore.dfy)
docs/        design notes
Tests/       corpus runner against tests/conformance.json (shared 139-case corpus)
```

## Status

Docs-only scaffold (Phase 6.2 of the master critical path) — no Swift code yet.
Implementation starts at 6.3 once the Dafny prerequisites (6.1: MSetList.dfy,
MCommand.dfy, MINIMAL-CONFORMING-M.md — merged via m-semantics#120) are on main.

## Build

SPM package; macOS 14+, Swift 5.10+. (No CI yet — added with the first code.)
