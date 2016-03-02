stalkernet-haskell
==================

A Haskell package for messing with the [Carleton directory](https://apps.carleton.edu/campus/directory/).

## Library (`src/`)

*   `Stalkernet.Types`: Types for people/staff/faculty, with...
    *   Binary serialization/deserialization
    *   JSON serialization/deserialization
    *   (ANSI-terminal colored) pretty-printer
*   `Stalkernet.Parsing`: Functions for extracting structured data from HTML
*   `Stalkernet.Fetching`: Functions for fetching data from the directory in multiple threads

## Executable (`exe/`)

Builds a binary called `stalk`.
For the time being, it is just a command-line interface to some of the library's features.
It may someday evolve into a full-featured search tool (possibly as a web service).

```
Usage: stalk fetch (binary|json|pretty)
       - fetch all data from stalkernet and
         write it to stdout in the given format

       stalk dump (binary|json) (binary|json|pretty)
       - read stalkernet date from stdin in the first
         format, and write it to stdout in the second
```

## Building

Run: `cabal install` in the root directory.

You need `ghc` and `cabal-install`.
The best way to obtain these tools are by installing the [Haskell platform](https://www.haskell.org/platform/).
`stalkernet.cabal` describes the build (flags, dependencies, etc.).
If you run into dependency issues (sometimes called 'cabal hell'), consider using [Nix](https://nixos.org/nix).

## Contributing

Yes! Please do.

## Extra Notes

Most TODO's in the source have a related note in TODO.md
