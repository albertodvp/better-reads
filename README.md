# better-reads

[![GitHub CI](https://github.com/albertodvp/better-reads/workflows/CI/badge.svg)](https://github.com/albertodvp/better-reads/actions)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

A toolkit to better handle goodread books.

- Random book pick
- Shelves reorg

## Development
Prepare a development environment.
```bash
pre-commit install
```
Enter the development shell.
NOTE: this is necessary for the following commands
```bash
nix-shell
```

### Run tests
```bash
nix-shell --command "cabal test"
```
### Run checks
```bash
# hlint
hlint src test app

# fourmolu check
fourmolu --mode check src test app

# fourmolu format
fourmolu --mode inplace src test app
```

### Build everything for dev purposes:
This builds the lib and all the executables.
```bash
stack build --file-watch
```

### Run the cli application
After a successful build, you can use the application from the command line:
```bash
stack run -- better-reads-cli --help
```

### Run the web application
The port will be set to `8123` by default, the environment variable `PORT` overrides the default port.
```bash
PORT=8080 stack run -- better-reads-web
```

## Deployment
### Local deployment
TODO
```bash
```
