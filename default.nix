{ nixpkgs ? import <nixpkgs> {}
, compiler ? "default"
, bench ? false
}:
let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                    then pkgs.haskellPackages
                    else pkgs.haskell.packages.${compiler};

  waarg = import ../waargonaut/waargonaut-deps.nix;

  modifiedHaskellPackages = haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: {}))
    (self: super: (waarg self super) // {
      waargonaut = self.callPackage ../waargonaut { inherit compiler bench; };
    });
  });

  drv = modifiedHaskellPackages.callPackage ./waargonaut-succinct-ds.nix {};

in
  drv
