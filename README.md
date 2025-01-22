# glaml

[![Package Version](https://img.shields.io/hexpm/v/glaml)](https://hex.pm/packages/glaml)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/glaml/)

Glaml is a simple Gleam wrapper around [yamerl](https://hex.pm/packages/yamerl) that enables your app to read YAML.

```gleam
import glaml

// ...

let assert Ok([doc]) = glaml.parse_string("
stars: 7
this-is-nil:
jobs:
  - being a cat
")

glaml.select_sugar(glaml.document_root(doc), "jobs.#0")
// -> Ok(NodeStr("being a cat"))
```

Further documentation can be found at <https://hexdocs.pm/glaml>.

## Instalation

You can add **glaml** to your project by simply running the command below.

```sh
gleam add glaml
```

## Development

```sh
./develop.sh # Copy Erlang headers (for lsp)
gleam test   # Run the tests
```
