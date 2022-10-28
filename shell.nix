let 
     pkgs = import <nixpkgs> { };
     hlsPkg = pkgs.fetchFromGitHub {
          owner = "shajra";
          repo = "haskell-hls-nix";
          rev = "0056e1096adae22fac8a5dfb272567f216f1d231";
          sha256="1qwiv199sib7f3qnf81xx3dk10nplcgrsqgvb0xp5i9psg1fxrak";
         # Descriptive name to make the store path easier to identify                
     };                                                                           

     #hls = pkgs.haskellPackages.callPackage "${hlsPkg}/default.nix" {ghcVersion = "8.6.5";};
     #hls = pkgs.haskellPackages.callPackage "${hlsPkg}/default.nix" {ghcVersion = "8.6.5";};
     hls = import "${hlsPkg}" {ghcVersion = "8.6.5";};
in
  pkgs.mkShell {
    buildInputs = with hls; [ 
      hls-wrapper
      hls-full
      hls-renamed
      cabal-install
      implicit-hie
    ];
  }

