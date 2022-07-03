<!--
 Copyright 2022 Joshua Wong.
 SPDX-License-Identifier: Apache-2.0 OR MIT
-->

# Lox Compiler written in Rust

A rust implementation of the [Lox Programming language](https://craftinginterpreters.com/the-lox-language.html).

## Setting up the environment

- Install the [Nix package manager](https://nixos.org/download.html)
- Enable [Flakes](https://nixos.wiki/wiki/Flakes)

    ```sh
    mkdir -p ~/.config/nix
    echo 'experimental-features = nix-command flakes' >> ~/.config/nix/nix.conf
    ```

- Install [Direnv](https://direnv.net/docs/installation.html) and [hook it into your shell](https://direnv.net/docs/hook.html)
- Allow direnv:

    ```sh
    direnv allow
    ```

Upon entering the project directory for the first time, nix will install all of the project's dependencies and recommended
development tools.
