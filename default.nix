{ chan ? "e1843646b04fb564abf6330a9432a76df3269d2f"
, compiler ? "ghc864"
, withHoogle ? false
, doHoogle ? false
, doHaddock ? false
, enableLibraryProfiling ? false
, enableExecutableProfiling ? false
, strictDeps ? false
, isJS ? false
, asShell ? false
}:
let


  # It's a shpadoinkle day
  shpadoinkle = builtins.fetchGit {
    url    = https://gitlab.com/fresheyeball/Shpadoinkle.git;
    rev    = "0b7d9fa744fc6a9be4a1ff1fc834c39ff9eae1f2";
    ref    = "master";
  };


  # Additional ignore patterns to keep the Nix src clean
  ignorance = [
    "*.md"
    "figlet"
    "*.nix"
    "*.sh"
    "*.yml"
  ];


  easy-hls = pkgs.callPackage (pkgs.fetchFromGitHub {
    owner  = "jkachmar";
    repo   = "easy-hls-nix";
    rev    = "db85cac9d0405b4769b75cba0b004aed3beaf2de";
    sha256 = "10nff6mqflrd6dz1fp2l9vmfwbgk0r7zm81qh2xnjj19a47pd7v3";
  }) {
    ghcVersions = [ "8.6.5" ];
  };


  # Get some utilities
  inherit (import (shpadoinkle + "/nix/util.nix") { inherit compiler isJS; }) compilerjs gitignore;


  # Build faster by doing less
  chill = p: (pkgs.haskell.lib.overrideCabal p {
    inherit enableLibraryProfiling enableExecutableProfiling;
  }).overrideAttrs (_: {
    inherit doHoogle doHaddock strictDeps;
  });


  # Overlay containing Shpadoinkle packages, and needed alterations for those packages
  # as well as optimizations from Reflex Platform
  shpadoinkle-overlay =
    import (shpadoinkle + "/nix/overlay.nix") { inherit compiler isJS; };


  # Haskell specific overlay (for you to extend)
  haskell-overlay = hself: hsuper: with pkgs.haskell.lib; { 
    "happy" = pkgs.haskell.lib.dontCheck hsuper.happy;
  };


  # Top level overlay (for you to extend)
  servant-client-js-overlay = self: super: {
    haskell = super.haskell //
      { packages = super.haskell.packages //
        { ${compilerjs} = super.haskell.packages.${compilerjs}.override (old: {
            overrides = super.lib.composeExtensions (old.overrides or (_: _: {})) haskell-overlay;
          });
        };
      };
    };


  # Complete package set with overlays applied
  pkgs = import
    (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/${chan}.tar.gz";
    }) {
    overlays = [
      shpadoinkle-overlay
      servant-client-js-overlay
    ];
  };


  # We can name him George
  servant-client-js = pkgs.haskell.packages.${compilerjs}.callCabal2nix "servant-client-js" (gitignore ignorance ./.) {};


in with pkgs; with lib; with haskell.packages.${compiler};

  if inNixShell || asShell
  then shellFor {
    inherit withHoogle;
    packages = _: [servant-client-js];
    COMPILER = compilerjs;
    buildInputs = [ easy-hls stylish-haskell cabal-install ghcid haddock ];
    shellHook = ''
      ${lolcat}/bin/lolcat ${./figlet}
    '';
  } else chill servant-client-js
