# Homework

The task is to recreate a portion of the functionality of the unix utility [`wc`](https://linux.die.net/man/1/wc).

## Functionality

1. `-c, --bytes` (2 point)
2. `-m, --chars` (2 point)
3. `-l, --lines` (4 points)

Properly parsing the CLI arguments into a separate data structure will give you 2 additional points for a total of 10.

Please see the [`wc` docs](https://linux.die.net/man/1/wc) for what every option should do.

The binary should accept one additional CLI argument -- a path to the file to read. You do not need to read from stdin.

The program should run in constant memory.

## Implementation

You should use the [`nightly-2024-10-11`](https://www.stackage.org/nightly-2024-10-11) stack snapshot in your project.

You should use the [streamly](https://hackage.haskell.org/package/streamly) family of packages for streaming functionality. Using a streaming package will help insure constant memory.

You should use the [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative) package to parse CLI arguments.
