# Repository Guidelines

## Project Structure & Module Organization
Numbered cheat sheets (e.g., `01_Internal_Tables.md`) at repo root act as canonical references; update them with the same change set as their runnable demos. ABAP runtime artifacts live under `src/` (see `src/zcl_demo_abap_string_proc.clas.abap`) and are the only files synced to systems through abapGit. Keep diagrams and screenshots in `files/` and leave compliance assets such as `LICENSE`, `LICENSES`, and `REUSE.toml` untouched except when legal terms change.

## Build, Test, and Development Commands
Work exclusively inside SAP BTP ABAP Environment or a classic system that matches the target branch. In ADT run the standalone `abapGit` report → *New Online* → `https://github.com/SAP-samples/abap-cheat-sheets.git` with folder logic `Full`; pick branch `main` for BTP tenants or the release-specific branches (`v757`, etc.) elsewhere. After pulling, choose *Activate all inactive ABAP development objects* (`Ctrl+Shift+F3`). Execute demo classes via `Run As → ABAP Application (Console)` (`F9`) and procedural programs via `Run As → ABAP Application` (`F8`).

## Coding Style & Naming Conventions
Reuse the ABAP Doc blocks already in place—HTML summaries plus references such as `{@link zcl_demo_abap_aux}` keep the documentation discoverable. Keywords stay uppercase, nested blocks share a two-space indent, and declaration lists use colon syntax (`DATA: flag TYPE c ...`). Classes follow the `zcl_demo_*` prefix, database artifacts use `zdemo_*`, interfaces rely on `zif_` or `zdemo_abap_*_itf`, and exception classes use `zcx_*`. Favor inline declarations (`DATA(result) = ...`), explicit numbering in console output, and avoid pragmas unless syntax coverage demands them.

## Testing Guidelines
See `14_ABAP_Unit_Tests.md`, `src/zcl_demo_abap_unit_test*.abap`, and `src/ztcl_demo_abap_unit_tdf_testcl*.abap` for canonical patterns. Local test classes are named `ltc_*`, marked `FOR TESTING`, and declare `RISK LEVEL` and `DURATION`. Shield dependent-on components with interfaces or test seams, create doubles inside the test include, and assert through `cl_abap_unit_assert`. Run suites using ADT’s `Run As → ABAP Unit Test` and attach console excerpts when filing issues.

## Commit & Pull Request Guidelines
Recent commits (“Add specialized migration skills…”) show the preferred style: a short, imperative subject that states the capability added, followed by focused diffs. Because the README asks external users not to open pull requests, report findings via issues that quote the relevant cheat sheet or class. If maintainers do raise a PR, include a description of affected documents and objects, screenshots for any asset change, and evidence of ABAP Unit or runtime validation.
