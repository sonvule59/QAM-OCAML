# Releasing

Checklist for cutting a release (example: v0.3.0).

1. **Commit & push everything**; wait for CI to go green on
   [Actions](https://github.com/sonvule59/QAM-OCAML/actions).
2. **Version**: bump `(version ...)` in `dune-project`, run `dune build`
   (regenerates `qam.opam`), update `CHANGELOG.md`, commit.
3. **Tag & release**:
   ```bash
   git tag v0.3.0
   git push origin main --tags
   ```
   Then on GitHub: *Releases → Draft a new release → choose the tag*, paste the
   CHANGELOG section, publish. (Optional: enable the Zenodo-GitHub integration
   first so the release mints a citable DOI; add it to CITATION.cff after.)
4. **Sanity-check the archive installs**:
   ```bash
   opam pin add qam https://github.com/sonvule59/QAM-OCAML/archive/refs/tags/v0.3.0.tar.gz
   qam run examples/qam/teleport.qam   # smoke test, then: opam unpin qam
   ```
5. **Publish to the opam repository** (makes `opam install qam` work for
   everyone). One-time: `opam install opam-publish`. Then:
   ```bash
   opam publish https://github.com/sonvule59/QAM-OCAML/archive/refs/tags/v0.3.0.tar.gz
   ```
   This opens a PR against `ocaml/opam-repository`; CI there runs
   `dune build` and the test suite (`ounit2` is a with-test dependency).
   Respond to reviewer comments on the PR; once merged, the package is live
   within a day.
6. **Announce**: OCaml discuss (https://discuss.ocaml.org, *Community →
   Announcements*) and, given the subject, consider emailing the QAM paper's
   authors — the artifact implements their machine and documents two fidelity
   findings (see CHANGELOG "Documented deviations").
