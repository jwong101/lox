# Copyright 2022 Joshua Wong.
# SPDX-License-Identifier: Apache-2.0 OR MIT

{
  description = "Rust Project skeleton using Naersk and Fenix";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    naersk = {
      url = "github:nix-community/naersk";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, flake-compat, naersk, fenix }:
    flake-utils.lib.eachDefaultSystem(system:
    let pkgs = nixpkgs.legacyPackages.${system};
        toolchain = {
          inherit (fenix.packages.${system}.complete) cargo rustc clippy rustfmt rust-src rust-analyzer-preview;
        };
        naersk-lib = naersk.lib.${system}.override {
          inherit (toolchain) cargo rustc;
        };
        pname = "rs-lox";
        package = naersk-lib.buildPackage {
          inherit pname;
          root = ./.;
        };
        app = flake-utils.lib.mkApp {
          drv = package;
        };
    in
    {
      packages.default = package;
      apps.default = app;
      devShells.default = pkgs.mkShell {
        buildInputs = with toolchain; [
          rustc
          cargo
          rust-analyzer-preview
          clippy
          rustfmt
        ];
        RUST_SRC_PATH = "${toolchain.rust-src}/lib/rustlib/src/rust/library";
      };
    });
}
