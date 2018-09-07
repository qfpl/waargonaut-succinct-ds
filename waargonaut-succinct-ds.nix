{ mkDerivation, base, bytestring, containers, contravariant, digit
, distributive, errors, hoist-error, hw-balancedparens, hw-bits
, hw-json, hw-prim, hw-rankselect, lens, mtl, nats, parsers
, scientific, semigroups, stdenv, text, transformers, vector
, waargonaut, witherable, wl-pprint-annotated, zippers
}:
mkDerivation {
  pname = "waargonaut-succinct-ds";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers contravariant digit distributive errors
    hoist-error hw-balancedparens hw-bits hw-json hw-prim hw-rankselect
    lens mtl nats parsers scientific semigroups text transformers
    vector waargonaut witherable wl-pprint-annotated zippers
  ];
  description = "JSON Mangling";
  license = stdenv.lib.licenses.bsd3;
}
