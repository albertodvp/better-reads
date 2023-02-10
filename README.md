# better-reads

[![GitHub CI](https://github.com/albertodvp/better-reads/workflows/CI/badge.svg)](https://github.com/albertodvp/better-reads/actions)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

A toolkit to better handle goodread books

- Random book pick
- Shelves reorg

### Setup
Prepare a development environment.

```bash
pre-commit install
nix-build
```

### Running tests
```bash
nix-shell
cabal test
```

### Running the cli application
After a successful build, you can use the application from the command line:
```bash
result/bin/better-reads-cli test_data/goodreads_library_export.csv output_to_import.csv --random --limit 5
```
