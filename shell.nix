{ nixpkgs ? import <nixpkgs> {}
, compiler ? "default"
, bench ? false
}:
(import ./default.nix { inherit compiler bench; }).env
