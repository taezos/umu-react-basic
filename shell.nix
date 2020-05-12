{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, ansi-terminal, base, bytestring
      , classy-prelude, errors, file-embed, microlens, microlens-th, mtl
      , optparse-applicative, stdenv, template-haskell, text
      , transformers, turtle, filepath
      }:
      mkDerivation {
        pname = "umu-react-basic";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          ansi-terminal base bytestring classy-prelude errors file-embed
          microlens microlens-th mtl optparse-applicative template-haskell
          text transformers turtle filepath
        ];
        executableHaskellDepends = [ base classy-prelude ];
        license = "unknown";
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
