{ mkDerivation, base, bytestring, digit, errors, hedgehog
, hoist-error, hw-balancedparens, hw-bits, hw-json, hw-prim
, hw-rankselect, lens, mtl, parsec, scientific, semigroups, stdenv
, tasty, tasty-expected-failure, tasty-hedgehog, tasty-hunit, text
, transformers, vector, waargonaut
}:
mkDerivation {
  pname = "waargonaut-succinct-ds";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring digit errors hoist-error hw-balancedparens hw-bits
    hw-json hw-prim hw-rankselect lens mtl scientific semigroups text
    transformers vector waargonaut
  ];
  testHaskellDepends = [
    base bytestring hedgehog lens parsec tasty tasty-expected-failure
    tasty-hedgehog tasty-hunit text waargonaut
  ];
  description = "JSON Mangling";
  license = stdenv.lib.licenses.bsd3;
}
