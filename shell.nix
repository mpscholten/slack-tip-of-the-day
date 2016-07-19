{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, classy-prelude, directory
      , http-conduit, http-types, linklater, postgresql-simple, stdenv
      , string-conversions, wai, wai-extra, wai-middleware-static, warp
      }:
      mkDerivation {
        pname = "slack-tip-of-the-day";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson base classy-prelude directory http-conduit http-types
          linklater postgresql-simple string-conversions wai wai-extra
          wai-middleware-static warp
        ];
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
