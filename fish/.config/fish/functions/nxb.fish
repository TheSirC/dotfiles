function nxb --description 'Use callPackage to build the nix expression passed as a parameter'
    nix-build -E "with import <nixpkgs> {}; callPackage $argv[1] {}"
end
