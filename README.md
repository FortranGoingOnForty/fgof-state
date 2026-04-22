# fgof-state

Persistent app and workspace state helpers for modern Fortran.

`fgof-state` is intended to be a small, standalone library for durable local
state, atomic save flows, and version-aware app data that CLI tools and
interactive apps keep hand-rolling.

It is part of the [FortranGoingOnForty lib-modules](https://github.com/FortranGoingOnForty/lib-modules)
catalog, but it is intended to stand on its own as a normal `fpm` package.

Current v1 target:

- stable state option, root, and document types
- predictable state-root resolution from explicit paths or XDG defaults
- atomic save or replace semantics for app and workspace state
- simple version-aware load or save flows with room for migrations later

Future scope:

- richer schema migration helpers
- backup or recovery helpers for state upgrades
- higher-level workspace and session persistence flows

## Status

Sprint 04 is in place.

Tracked today:

- explicit state-root resolution from `root_dir`, `XDG_STATE_HOME`, or `HOME`
- namespace and scope-aware root layout
- document-path helpers and document resolution
- text-focused state save, load, and remove helpers
- `fgof-temp`-backed atomic replace semantics for saves
- side-effect-free reads and removes when state roots are missing
- version-aware state envelopes with explicit mismatch handling
- malformed or unsupported state payloads rejected as version errors
- invalid version arguments rejected before filesystem writes
- tracked round-trip and version-mismatch examples
- POSIX root creation and existence checks
- focused scaffold coverage in `fpm test`
- CI on macOS and Ubuntu, including direct example execution

## Why Use It

- app and workspace state shows up everywhere once tools grow past trivial size
- atomic persistence is easy to get subtly wrong
- a focused state layer builds naturally on the released temp and cache modules

## Public API Shape

Primary modules:

- `fgof_state`
- `fgof_state_types`

Public types:

- `state_options`
- `state_root`
- `state_document`
- `state_text_result`

Public constants:

- `FGOF_STATE_OK`
- `FGOF_STATE_ERR_INVALID_OPTIONS`
- `FGOF_STATE_ERR_NOT_FOUND`
- `FGOF_STATE_ERR_IO`
- `FGOF_STATE_ERR_VERSION`
- `FGOF_STATE_ERR_INTERNAL`

Current public procedures:

- `clear_state_options`
- `clear_state_root`
- `clear_state_document`
- `clear_state_text_result`
- `ensure_state_root`
- `save_state_text`
- `load_state_text`
- `remove_state_document`
- `state_relative_path_for_name`
- `state_path_for_name`
- `resolve_state_document`
- `state_backend_name`
- `state_error_name`

Current semantics:

- `ensure_state_root()` resolves from explicit `root_dir`, or from `XDG_STATE_HOME` / `HOME` when `root_dir` is absent
- explicit `namespace` and `scope` values extend the resolved root path as validated path segments
- `create_root=.true.` creates state directories on demand; `create_root=.false.` requires the root to already exist
- `state_relative_path_for_name()` and `state_path_for_name()` expose the stable document path shape for valid document names
- `resolve_state_document()` returns the resolved root path, relative path, full path, and current presence state for a state document
- `save_state_text()` writes exact Fortran character payloads with same-directory atomic replacement through `fgof-temp`
- `load_state_text()` reads stored text back without creating missing roots as a side effect
- `remove_state_document()` removes stored state files without creating missing roots as a side effect
- `save_state_text()` stores a small internal format header plus a positive document version; the default version is `1`
- `load_state_text()` surfaces the stored version on `state_text_result%document%version`
- `load_state_text(..., expected_version=...)` reports `version` errors explicitly when the stored document version does not match the caller expectation
- unsupported or malformed state payloads also report `version` errors instead of pretending to be valid text
- missing documents report `not-found` for load or remove flows
- document names, namespaces, and scopes must not be empty, contain `/`, or be `.` / `..`

## Build And Test

```bash
fpm test
```

That is the baseline verification command locally and in CI.

Tracked examples:

- [state_roundtrip_demo.f90](example/state_roundtrip_demo.f90)
- [state_version_demo.f90](example/state_version_demo.f90)

## Supported Platforms

- macOS
- Linux

## Boundaries

- intended to stay independently versioned and releasable
- focused on state ergonomics, not full database or cache policy management
- should stay useful on its own even if future higher-level app packages build on top

## License

MIT
