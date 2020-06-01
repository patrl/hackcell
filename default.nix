let
  haskellCompiler = "ghc883";
  sources = import ./nix/sources.nix;
  ghcide = (import sources.ghcide-nix {})."ghcide-${haskellCompiler}";
  haskellNix = import sources.iohk-hnix {};
  nixpkgsSrc = haskellNix.sources.nixpkgs-2003;
  nixpkgsArgs = haskellNix.nixpkgsArgs;
  pkgs = import nixpkgsSrc nixpkgsArgs;

  hspkgs = pkgs.haskell-nix.cabalProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { name = "myProject"; src = ./.; };
    compiler-nix-name = haskellCompiler;
  };

  shell = hspkgs.shellFor {
    withHoogle = true;
    buildInputs = [
      pkgs.cabal-install
      pkgs.haskellPackages.ghcid
      pkgs.haskellPackages.hpack
      pkgs.haskellPackages.brittany
      ghcide
    ];
  };
in
{
  inherit shell;
  inherit hspkgs;
  myProject = hspkgs.myProject;
}
