set shell := ["sh", "-eu", "-c"]

hs := "app src test"

default:
    @just --list

# Format everything in place: flake.nix, justfile, Haskell
fmt:
    nixfmt flake.nix
    just --fmt --unstable
    fourmolu --mode inplace {{ hs }}

# Format drift, then hlint with relude's hints
lint:
    nixfmt --check flake.nix
    just --fmt --check --unstable >/dev/null
    fourmolu --mode check {{ hs }}
    hlint --hint=.hlint.yaml --hint="$RELUDE_HLINT" {{ hs }}

# Full gate: lint, strict build, tests, dead code, anti-patterns
check: lint
    cabal build all
    cabal test all
    weeder --hie-directory .hie
    env -u STAN_USE_DEFAULT_CONFIG stan --no-default --short
    env -u STAN_USE_DEFAULT_CONFIG stan --no-default --json-output >.hie/stan.json
    jq -e '.observations == []' .hie/stan.json >/dev/null
