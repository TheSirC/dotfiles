function nxb
    nix-build -E "with import <nixpkgs> {}; callPackage $argv[1] {}"
end
