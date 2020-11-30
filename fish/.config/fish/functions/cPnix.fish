function cPnix
    nix-build -E "with import <nixpkgs> {}; callPackage $argsv[1] {}"
end
