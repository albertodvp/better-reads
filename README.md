# better-reads

[![GitHub CI](https://github.com/albertodvp/better-reads/workflows/CI/badge.svg)](https://github.com/albertodvp/better-reads/actions)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

A toolkit to better handle goodread books. A live pre-pre-pre-alpha version can be found [here](http://ec2-3-123-153-53.eu-central-1.compute.amazonaws.com)

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
cabal test
```

### Running the cli application
After a successful build, you can use the application from the command line:
```bash
result/bin/better-reads-cli test_data/goodreads_library_export.csv output_to_import.csv --random --limit 5
```
