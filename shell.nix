let
  project = import ./default.nix;
in
  project.shellFor {

    packages = ps: with ps; [
      mantra-oracle
    ];

    # Set the following to `false` do disable the lengthy building of documentation.
    withHoogle = false;

     # See overlays/tools.nix for more details
    tools = {
      cabal                   = "latest";
      haskell-language-server = "latest";
      hlint                   = "latest";
      pointfree               = "latest";
    # pointfull               = "latest";
    };

    buildInputs = [ (import <nixpkgs> {}).git ];

    exactDeps = true;
  }
