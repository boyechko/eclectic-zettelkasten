# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

### Added

- `CHANGELOG.md` with major milestones.

### Refactored

- Begin modularizing `ezeka.el` (was 5361 lines) to improve maintainability.
- Extract 2009 lines of code from `ezeka.el` into:
  - `ezeka-base.el`
  - `ezeka-file.el`
  - `ezeka-meta.el`

## [0.1.1] - 2022-08-31

### Changed

- Rename "combined title" metadata key `title` to `rubric`.

## [0.1.0] - 2022-08-17

### Added

- Eclectic Zettelkasten is officially born.
- `ezeka-zk.el` for integration with Grant Rosson's Zk and Zk-Index.

### Changed

- Rename package from `zettel` to `ezeka` (i.e. Eclectic Zettelkasten)
- Split code relying on other packages into their own files:
  - `ezeka-citar.el`
  - `ezeka-deft.el`
  - `ezeka-ivy.el`
  - `ezeka-md.el`

## [0.0.1] - 2015-08-31

### Changed

- Move Zettelkasten-related code to `zettel.el`.
- Create Git repository.
