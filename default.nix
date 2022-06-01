{ rustPlatform
, pkg-config
, openssl
, lib
}:

rustPlatform.buildRustPackage {
  pname = "nix-doc-munge";
  version = "0.1.0";

  src = lib.cleanSource ./.;

  nativeBuildInputs = [ ];
  buildInputs = [ ];

  cargoLock.lockFile = ./Cargo.lock;
}
