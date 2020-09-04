# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),

Versions prior to v0.1.0 are considered experimental, their API may change.

## [0.0.4] - 2020-09-04

### Fixed
- Cljdoc fixes

## [0.0.3] - 2020-09-04

### Added
- A new diff type `:missing` was added, it represents data which was removed at the top level.
  It's like the complement of the `:value` type which is also only used at the root level.

### Changed
- The data format of the map-dissoc operation was changed to simplify the code which is parsing the diffs.
  This change does not break programs which were only using the helpers to create the diffs.

### Deps
- The dependency on Minimallist was moved in the extra-deps of the :test alias.
  If you want to use `diffuse.model/diff-model`, you will need to manually include the dependency on Minimallist.

## [0.0.2] - 2020-08-07

### Added
- this changelog file.
- the `let` macro in the helper namespace.

## [0.0.1] - 2020-08-07

### Added
- the `apply` and `comp` functions.
- the helper functions, to build the diffs.
