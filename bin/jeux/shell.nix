{ pkgs ? import <nixpkgs> { } }:

with pkgs;

mkShell {
  buildInputs = [
    (wineWowPackages.full.override {
      wineRelease = "staging";
      mingwSupport = true;
    })
    (winetricks.override {
      wine = wineWowPackages.staging;
    })
  ];
  shellHook = ''
    export WINEPREFIX=~/Jeux/epic-games-store/SatisfactoryExperimental/.wine;
    export WINEARCH=win64;
    winecfg;
    winetricks dxvk mf;
    bin/jeux/satisfactory.sh
  '';
}
