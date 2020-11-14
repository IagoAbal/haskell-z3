{ config ? { allowBroken = true; }
, ...
}:
let
  #nixpkgs = import <nixpkgs> { inherit config; };
  nixpkgs = import
    (builtins.fetchTarball {
      # latest https://github.com/NixOS/nixpkgs/commits/nixos-20.09 as of Sun 01 Nov 2020 10:25:16 PM UTC
      url = "https://github.com/NixOS/nixpkgs/archive/edb26126d98bc696f4f3e206583faa65d3d6e818.tar.gz";
      sha256 = "1cl4ka4kk7kh3bl78g06dhiidazf65q8miyzaxi9930d6gwyzkci";
    })
    { inherit config; };
  # package set for haskell compiler version
  haskellCompilerPackages = nixpkgs.haskellPackages; # don't care about compiler version
  # override package set to inject project components
  haskellPackages = haskellCompilerPackages.override
    (old: {
      overrides = self: super: with nixpkgs.haskell.lib; rec {
        gomp = self.hello;
      };
    });
  # helper functions
  withSysdeps = pkg: nixpkgs.haskell.lib.overrideCabal pkg (old: { buildTools = old.buildTools or [ ] ++ [ nixpkgs.z3 ]; });
  withDevtools = pkg: pkg.overrideAttrs (old: { nativeBuildInputs = old.nativeBuildInputs or [ ] ++ [ nixpkgs.ghcid ]; });
  gitignoredSrc = path: nixpkgs.nix-gitignore.gitignoreSource [ ] path;
  # derivation
  drv = withSysdeps (haskellPackages.callCabal2nix "z3" (gitignoredSrc ./.) { });
in
if nixpkgs.lib.inNixShell then withDevtools drv.env else drv
