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

Scaffold is in place.

Tracked today:

- starter state option, root, and document types
- placeholder backend and error helpers
- focused scaffold coverage in `fpm test`
- CI on macOS and Ubuntu

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
- `state_backend_name`
- `state_error_name`

## Build And Test

```bash
fpm test
```

That is the baseline verification command locally and in CI.

## Supported Platforms

- macOS
- Linux

## Boundaries

- intended to stay independently versioned and releasable
- focused on state ergonomics, not full database or cache policy management
- should stay useful on its own even if future higher-level app packages build on top

## License

MIT
