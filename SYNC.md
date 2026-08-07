# Keeping the paper submodule in sync

The manuscript in `paper/` is a submodule pointing at
[GRH-Paper-JAWRA-2026](https://github.com/cameronbracken/GRH-Paper-JAWRA-2026).

## Direction of truth

```
Overleaf  ──(manual push)──>  GitHub  ──(git pull)──>  paper/
  authoring surface            mirror              local checkout
```

**Overleaf is where the paper is written. GitHub is a mirror, and the push is manual.**

This is the thing to remember: `git pull` inside `paper/` fetches only what has been pushed
*from* Overleaf. If edits were made in Overleaf and not pushed, git cannot see them and will
report everything up to date. That is not a sync error, it is the expected behavior of a
one-way manual mirror.

This bit us once already, on 2026-08-06: the GitHub copy sat four hunks behind the Overleaf
working copy during the revision, and a naive clone would have silently reverted those edits.

## Before editing the paper locally

1. Push from Overleaf first (Overleaf → Menu → GitHub → Push).
2. Then, in this repo:
   ```bash
   git submodule update --remote paper
   ```
3. Confirm the submodule moved to the expected commit:
   ```bash
   git -C paper log -1 --oneline
   ```

## After editing the paper locally

Local edits in `paper/` do **not** flow back to Overleaf automatically. Either:

- Push from `paper/` to GitHub and pull into Overleaf from its GitHub menu, or
- Make the edit in Overleaf instead and push, which is usually simpler.

Then record the new submodule pointer in this repo:
```bash
git add paper && git commit -m "Bump paper submodule"
```

## Building the paper locally

```bash
cd paper
pdflatex main.tex && bibtex main && pdflatex main.tex && pdflatex main.tex
```

Expect 22 pages with no undefined references or citations.

**Local-only issue:** `WileyNJDv5.cls` calls `\reserveinserts`, which `etex` no longer defines
in TeX Live 2026, so `pdflatex` aborts. Overleaf is unaffected. To build locally, prepend a
shim rather than editing the class file:

```bash
{ printf '\\providecommand{\\reserveinserts}[1]{}\n'; cat main.tex; } > _local.tex
pdflatex _local.tex   # etc.
```

The unmodified file fails the same way, so this is not caused by any edit to the manuscript.

## Analysis code and vendor anonymity

The analysis code in this repo is the **public** version and uses `A` and `B` for the two
commercial forecast products. That anonymization is contractual. The working directory at
`~/projects/h2o/grh` still uses the raw source names, so porting code here means translating,
not copying. Before any push:

```bash
# -s skips the submodule dir; ':!*.md' stops this document's own warning self-matching
git ls-files -- ':!*.md' | xargs grep -siEl 'hatch|upstr'"eam"   # must return nothing
```

See `~/projects/h2o/grh/README.md` for details.
