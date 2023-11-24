# better-reads

[![GitHub CI](https://github.com/albertodvp/better-reads/workflows/CI/badge.svg)](https://github.com/albertodvp/better-reads/actions)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

A toolkit to better handle goodread books.

- Random book pick
- Shelves reorg

## Development
I suggest using [direnv](https://direnv.net/), this would work with nix seamlessly.

### Run tests
```bash
cabal test
```
### Run checks
```bash
# hlint
hlint src test app

# fourmolu check
ormolu --mode check $(find . -name '*.hs')

# fourmolu format
ormolu --mode inplace $(find . -name '*.hs')
```

### Run the cli application
After a successful build, you can use the application from the command line:
```bash
cabal run better-reads-cli --help
```

### Run the web application
The port will be set to `8123` by default, the environment variable `PORT` overrides the default port.
```bash
PORT=8080 cabal run better-reads-web
```
